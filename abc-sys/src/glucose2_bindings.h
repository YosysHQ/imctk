#ifndef IMCTK_ABC_SYS_GLUCOSE2_BINDINGS_H
#define IMCTK_ABC_SYS_GLUCOSE2_BINDINGS_H

#include "misc/util/abc_namespaces.h"
#include <stddef.h>
#include <stdint.h>

ABC_NAMESPACE_HEADER_START

void *imctk_abc_glucose2_init();

void imctk_abc_glucose2_release(void *s);

void imctk_abc_glucose2_reset(void *s);

void imctk_abc_glucose2_set_incremental_mode(void *s);

int imctk_abc_glucose2_new_var(void *s);

int imctk_abc_glucose2_nvars(void *s);

int imctk_abc_glucose2_add_clause(void *s, const int *lits, int nlits);

int imctk_abc_glucose2_solve_limited(void *s, const int *lits, int nlits,
                                     int do_simp, int turn_off_simp);

int *imctk_abc_glucose2_get_cex(void *s);

void imctk_abc_glucose2_produce_inner_model(void *s, int produce_inner);

int imctk_abc_glucose2_value(void *s, int var);

void imctk_abc_glucose2_markapprox(void *s, int v0, int v1, int nlim);

int imctk_abc_glucose2_jftr(void *s);

void imctk_abc_glucose2_set_jftr(void *s, int jftr);

void imctk_abc_glucose2_set_var_fanin_lit(void *s, int var, int lit0, int lit1);

void imctk_abc_glucose2_start_new_round(void *s);

void imctk_abc_glucose2_mark_cone(void *s, int var);

void imctk_abc_glucose2_mark_var(void *s, int var);

void imctk_abc_glucose2_prelocate(void *s, int var_num);

int64_t imctk_abc_glucose2_conflicts(void *s);

void imctk_abc_glucose2_set_conf_budget(void *s, int64_t budget);

void imctk_abc_glucose2_budget_off(void *s);

void imctk_abc_glucose2_start_proof_trace(
    void *s, void *data,
    uint32_t (*learnt_clause)(void *data, const int *lits, int nlits,
                              const uint32_t *tags, int ntags, const int *units,
                              int nunits),
    void (*learnt_unit)(void *data, int lit, uint32_t tag, const int *units,
                        int nunits),
    void (*conflict)(void *data, const int *lits, int nlits,
                     const uint32_t *tags, int ntags, const int *units,
                     int nunits));

void imctk_abc_glucose2_stop_proof_trace(void *s);

void imctk_abc_glucose2_proof_trace_set_default_tag(void *s, uint32_t tag);

uint32_t imctk_abc_glucose2_proof_trace_default_tag(void *s);

int imctk_abc_glucose2_conflict_size(void *s);

const int *imctk_abc_glucose2_conflict_lits(void *s);

ABC_NAMESPACE_HEADER_END

#endif
