//! Default logging setup for the Incremental Model Checking Toolkit
#![deny(unsafe_op_in_unsafe_fn)]
#![warn(clippy::undocumented_unsafe_blocks)]
#![warn(missing_docs)]

use std::{cell::RefCell, fmt, sync::atomic::AtomicUsize};

#[derive(Debug)]
struct RssStats {
    current: MemoryAmount,
    max: MemoryAmount,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct MemoryAmount(usize);

impl fmt::Debug for MemoryAmount {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self, f)
    }
}

impl fmt::Display for MemoryAmount {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0 < 1000 {
            write!(f, "{:5}B", self.0)
        } else if self.0 < 1000 << 10 {
            write!(f, "{:5.1}K", self.0 as f64 / (1u64 << 10) as f64)
        } else if self.0 < 1000 << 20 {
            write!(f, "{:5.1}M", self.0 as f64 / (1u64 << 20) as f64)
        } else {
            write!(f, "{:5.1}G", self.0 as f64 / (1u64 << 30) as f64)
        }
    }
}

struct ProcStatusData {
    file: std::fs::File,
    buf: Vec<u8>,
    pagesize: usize,
}

#[cfg(target_os = "linux")]
thread_local! {
    static STATUS_FD: RefCell<Option<ProcStatusData>> = const { RefCell::new(None) };
}

impl RssStats {
    fn now() -> Self {
        #[cfg(not(miri))]
        {
            #[cfg(target_os = "macos")]
            {
                // SAFETY: rusage is plain old data so all zeros is valid
                let mut rusage = unsafe { std::mem::zeroed() };
                // SAFETY: getrusage is safe to call as long as it can safely write to the
                if unsafe { libc::getrusage(libc::RUSAGE_SELF, &mut rusage) } < 0 {
                    panic!("getrusage faield")
                }

                // SAFETY: proc_taskallinfo is plain old data so all zeros is valid
                let mut info: libc::proc_taskallinfo = unsafe { std::mem::zeroed() };

                // SAFETY: proc_pidinfo is safe to call as long as it can safely write to the passed
                // pointer and is given the correct storage size for the passed pointer
                if unsafe {
                    libc::proc_pidinfo(
                        libc::getpid(),
                        libc::PROC_PIDTASKALLINFO,
                        0,
                        ((&mut info) as *mut libc::proc_taskallinfo).cast::<libc::c_void>(),
                        std::mem::size_of::<libc::proc_taskallinfo>() as i32,
                    )
                } < std::mem::size_of::<libc::proc_taskallinfo>() as i32
                {
                    panic!("proc_pidinfo failed");
                }

                info.ptinfo.pti_resident_size;
                Self {
                    max: MemoryAmount(rusage.ru_maxrss as usize),
                    current: MemoryAmount(info.ptinfo.pti_resident_size as usize),
                }
            }
            #[cfg(target_os = "linux")]
            {
                STATUS_FD.with_borrow_mut(|status_data| {
                    let status_data: &mut ProcStatusData =
                        status_data.get_or_insert_with(|| ProcStatusData {
                            file: std::fs::File::open("/proc/self/statm").unwrap(),
                            buf: Default::default(),
                            // SAFETY: standard way to obtain page size
                            pagesize: unsafe { libc::sysconf(libc::_SC_PAGESIZE) as usize },
                        });

                    // SAFETY: rusage is plain old data so all zeros is valid
                    let mut rusage = unsafe { std::mem::zeroed() };
                    // SAFETY: getrusage is safe to call as long as it can safely write to the
                    // passed pointer
                    if unsafe { libc::getrusage(libc::RUSAGE_SELF, &mut rusage) < 0 } {
                        panic!("getrusage faield")
                    }
                    use std::io::Seek;
                    status_data.file.seek(std::io::SeekFrom::Start(0)).unwrap();
                    status_data.buf.clear();
                    std::io::copy(&mut status_data.file, &mut status_data.buf).unwrap();

                    let rss_pages = std::str::from_utf8(&status_data.buf)
                        .unwrap()
                        .split_ascii_whitespace()
                        .nth(1)
                        .unwrap()
                        .parse::<usize>()
                        .unwrap();

                    Self {
                        max: MemoryAmount(rusage.ru_maxrss as usize * 1024),
                        current: MemoryAmount(rss_pages * status_data.pagesize),
                    }
                })
            }
        }
        #[cfg(miri)]
        {
            Self {
                current: MemoryAmount(0),
                max: MemoryAmount(0),
            }
        }
    }
}

const TIMESTAMP_STYLE: anstyle::Style =
    anstyle::Style::new().fg_color(Some(anstyle::Color::Ansi(anstyle::AnsiColor::BrightBlack)));

const MEMORY_STYLE: anstyle::Style =
    anstyle::Style::new().fg_color(Some(anstyle::Color::Ansi(anstyle::AnsiColor::Blue)));
const MEMORY_NEW_PEAK_STYLE: anstyle::Style =
    anstyle::Style::new().fg_color(Some(anstyle::Color::Ansi(anstyle::AnsiColor::Red)));
const MEMORY_PEAK_STYLE: anstyle::Style =
    anstyle::Style::new().fg_color(Some(anstyle::Color::Ansi(anstyle::AnsiColor::BrightBlack)));

const TARGET_STYLE: anstyle::Style =
    anstyle::Style::new().fg_color(Some(anstyle::Color::Ansi(anstyle::AnsiColor::Magenta)));

/// Perform the default logging setup used by imctk examples
pub fn setup() {
    let start_time = std::time::Instant::now();
    let peak = AtomicUsize::new(RssStats::now().max.0);

    let last_target = std::sync::Mutex::new(String::new());

    env_logger::Builder::from_env(
        env_logger::Env::new()
            .filter_or("IMCTK_LOG", "info")
            .write_style("IMCTK_LOG_STYLE"),
    )
    .format(move |buf, record| {
        use std::io::Write;

        // buf.timestamp().
        let timestamp = start_time.elapsed();
        let level = record.level();
        let target = record.target();

        let RssStats { current, max } = RssStats::now();

        let new_peak = peak.fetch_max(max.0, std::sync::atomic::Ordering::Relaxed) < max.0;

        let mut last_target = last_target.lock().unwrap();

        if target != *last_target {
            last_target.clear();
            last_target.push_str(target);

            writeln!(
                buf,
                "{} {} {} {}",
                format_args!("{style}{timestamp:>9.2?}{style:#}", style = TIMESTAMP_STYLE),
                format_args!("{style}{current}{style:#}", style = MEMORY_STYLE),
                format_args!(
                    "{style}{max}{style:#}",
                    style = if new_peak {
                        MEMORY_NEW_PEAK_STYLE
                    } else {
                        MEMORY_PEAK_STYLE
                    }
                ),
                format_args!("{style}{target}{style:#}", style = TARGET_STYLE)
            )?;
        }
        writeln!(
            buf,
            "{} {} {} {} {}",
            format_args!("{style}{timestamp:>9.2?}{style:#}", style = TIMESTAMP_STYLE),
            format_args!("{style}{current}{style:#}", style = MEMORY_STYLE),
            format_args!(
                "{style}{max}{style:#}",
                style = if new_peak {
                    MEMORY_NEW_PEAK_STYLE
                } else {
                    MEMORY_PEAK_STYLE
                }
            ),
            format_args!(
                "{style}{level}{style:#}",
                style = buf.default_level_style(level),
            ),
            record.args(),
        )
    })
    .init();
}
