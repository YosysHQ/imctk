#ifndef IMCTK_ABC_SYS_BINDINGS_H
#define IMCTK_ABC_SYS_BINDINGS_H

#include "aig/gia/gia.h"
#include "aig/gia/giaAig.h"
#include "base/abc/abc.h"
#include "base/main/main.h"
#include "misc/util/abc_global.h"
#include "proof/cec/cec.h"
#include "proof/pdr/pdr.h"
#include "proof/pdr/pdrInt.h"

#include "glucose2_bindings.h"

#ifdef __cplusplus
extern "C" {
#endif

extern unsigned int enable_dbg_outs;

void Cec4_ManSetParams(Cec_ParFra_t *pPars);
Gia_Man_t *Cec5_ManSimulateTest(Gia_Man_t *p, Cec_ParFra_t *pPars, int fCbs,
                                int approxLim, int subBatchSz,

                                int adaRecycle);

void imctk_abc_line_buffer_stdout();

#ifdef __cplusplus
}
#endif

#endif
