#ifndef IMCTK_ABC_SYS_BINDINGS_H
#define IMCTK_ABC_SYS_BINDINGS_H

#include "aig/gia/gia.h"
#include "misc/util/abc_global.h"
#include "proof/cec/cec.h"

#ifdef __cplusplus
extern "C" {
#endif

void Cec4_ManSetParams(Cec_ParFra_t *pPars);
Gia_Man_t *Cec5_ManSimulateTest(Gia_Man_t *p, Cec_ParFra_t *pPars,
                                       int fCbs, int approxLim, int subBatchSz,

                                       int adaRecycle);

#ifdef __cplusplus
}
#endif

#endif
