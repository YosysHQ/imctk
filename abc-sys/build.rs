use std::path::PathBuf;

static SRC_FILES: &[&str] = &[
    "abc/src/aig/gia/gia.h",
    "abc/src/aig/gia/giaCSatP.c",
    "abc/src/aig/gia/giaDup.c",
    "abc/src/aig/gia/giaEquiv.c",
    "abc/src/aig/gia/giaExist.c",
    "abc/src/aig/gia/giaFanout.c",
    "abc/src/aig/gia/giaHash.c",
    "abc/src/aig/gia/giaMan.c",
    "abc/src/aig/gia/giaScl.c",
    "abc/src/aig/gia/giaSim.c",
    "abc/src/aig/gia/giaUtil.c",
    "abc/src/base/main/mainFrame.c",
    "abc/src/base/main/mainReal.c",
    "abc/src/misc/mem/mem.c",
    "abc/src/misc/tim/timMan.c",
    "abc/src/misc/util/utilBridge.c",
    "abc/src/misc/util/utilCex.c",
    "abc/src/misc/util/utilFile.c",
    "abc/src/misc/util/utilSort.c",
    "abc/src/proof/cec/cecSatG2.c",
    "abc/src/proof/cec/cecSatG3.c",
    "abc/src/sat/glucose2/AbcGlucose2.cpp",
    "abc/src/sat/glucose2/AbcGlucoseCmd2.cpp",
    "abc/src/sat/glucose2/Glucose2.cpp",
    "abc/src/sat/glucose2/Options2.cpp",
    "abc/src/sat/glucose2/SimpSolver2.cpp",
    "abc/src/sat/glucose2/System2.cpp",
    "src/bindings.cpp",
    "src/glucose2_bindings.cpp",
];

fn main() {
    if std::path::Path::new("codegen.sh").exists() {
        let status = std::process::Command::new("sh")
            .arg("codegen.sh")
            .spawn()
            .expect("couldn't run codegen script")
            .wait()
            .expect("couldn't run codegen script");
        assert!(status.code() == Some(0), "codegen script failed");
    }

    if std::env::var("NO_SCCACHE").is_err()
        && std::process::Command::new("sccache")
            .arg("--version")
            .status()
            .is_ok()
    {
        std::env::set_var(
            "CC",
            format!("sccache {}", std::env::var("CC").as_deref().unwrap_or("cc")),
        );
        std::env::set_var(
            "CXX",
            format!(
                "sccache {}",
                std::env::var("CXX").as_deref().unwrap_or("c++")
            ),
        );
    }

    let mut cc = cc::Build::new();

    cc.warnings(false)
        .define("ABC_USE_STDINT_H", "1")
        .flag_if_supported("-Wno-unused-function")
        .flag_if_supported("-Wno-write-strings")
        .flag_if_supported("-Wno-sign-compare")
        .include("abc/src")
        .include(".");

    let mut cc_c = cc.clone();
    let mut cc_cpp = cc;
    cc_cpp.cpp(true);

    cc_cpp.files(SRC_FILES.iter().filter(|&f| f.ends_with(".cpp")));
    cc_c.files(SRC_FILES.iter().filter(|&f| f.ends_with(".c")));

    for file in SRC_FILES {
        println!("cargo::rerun-if-changed={file}");
    }

    let mut deps_dir = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    deps_dir.push("deps");

    let cc_deps = [cc_cpp.clone(), cc_c.clone()];

    cc_cpp.objects(cc_c.compile_intermediates());
    cc_cpp.compile("imctk-abc");

    for mut cc_dep in cc_deps {
        cc_dep.out_dir(&deps_dir);
        let Ok(dep_files) = cc_dep.flag("-M").try_compile_intermediates() else {
            continue;
        };

        for path in dep_files {
            let deps = std::fs::read_to_string(path).unwrap();

            let Some((_c_file, headers)) = deps.split_once(':') else {
                continue;
            };

            for header in headers
                .split_ascii_whitespace()
                .filter(|&part| !(part == "\\" || part.starts_with("/")))
            {
                println!("cargo::rerun-if-changed={header}");
            }
        }
    }
}
