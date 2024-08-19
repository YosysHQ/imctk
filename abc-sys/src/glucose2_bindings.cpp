#include "misc/util/abc_namespaces.h"

#include "glucose2_bindings.h"

#include "sat/glucose2/SimpSolver.h"

ABC_NAMESPACE_IMPL_START

using namespace Gluco2;

static int lboolToInt(lbool v) {
  return v == l_Undef ? 0 : v == l_True ? 1 : -1;
}

class WrappedSolver : public SimpSolver {
public:
  bool addClause(const int *lits, int nlits) {
    add_tmp.shrink_(add_tmp.size());
    add_tmp.growTo(nlits);
    memcpy(add_tmp, lits, sizeof(int) * nlits);
    return addClause_(add_tmp);
  }
};

#define solver ((WrappedSolver *)s)

void *imctk_abc_glucose2_init() { return (void *)new WrappedSolver; }

void imctk_abc_glucose2_release(void *s) { delete solver; }

void imctk_abc_glucose2_reset(void *s) { solver->reset(); }

void imctk_abc_glucose2_set_incremental_mode(void *s) {
  solver->setIncrementalMode();
}

int imctk_abc_glucose2_new_var(void *s) { return solver->newVar(); }

int imctk_abc_glucose2_nvars(void *s) { return solver->nVars(); }

int imctk_abc_glucose2_add_clause(void *s, const int *lits, int nlits) {
  return solver->addClause(lits, nlits);
}

int imctk_abc_glucose2_solve_limited(void *s, const int *lits, int nlits,
                                     int do_simp, int turn_off_simp) {
  return solver->solveLimited((int *)lits, nlits, do_simp, turn_off_simp);
}

int *imctk_abc_glucose2_get_cex(void *s) { return solver->getCex(); }

void imctk_abc_glucose2_produce_inner_model(void *s, int produce_inner) {
  solver->produceInnerModel = produce_inner;
}

int imctk_abc_glucose2_value(void *s, int var) {
  return lboolToInt(solver->value(var));
}

void imctk_abc_glucose2_markapprox(void *s, int v0, int v1, int nlim) {
  solver->markApprox(v0, v1, nlim);
}

int imctk_abc_glucose2_jftr(void *s) { return solver->jftr; }

void imctk_abc_glucose2_set_jftr(void *s, int jftr) { solver->jftr = jftr; }

void imctk_abc_glucose2_set_var_fanin_lit(void *s, int var, int lit0,
                                          int lit1) {
  solver->sat_solver_set_var_fanin_lit(var, lit0, lit1);
}

void imctk_abc_glucose2_start_new_round(void *s) {
  solver->sat_solver_start_new_round();
}

void imctk_abc_glucose2_mark_cone(void *s, int var) {
  solver->sat_solver_mark_cone(var);
}

void imctk_abc_glucose2_mark_var(void *s, int var) {
  solver->sat_solver_mark_var(var);
}

void imctk_abc_glucose2_prelocate(void *s, int var_num) {
  solver->prelocate(var_num);
}

int64_t imctk_abc_glucose2_conflicts(void *s) {
  return solver->conflicts;
}

void imctk_abc_glucose2_set_conf_budget(void *s, int64_t budget) {
  solver->setConfBudget(budget);
}

void imctk_abc_glucose2_budget_off(void *s) { solver->budgetOff(); }

void imctk_abc_glucose2_start_proof_trace(
    void *s, void *data,
    uint32_t (*learnt_clause)(void *data, const int *lits, int nlits,
                              const uint32_t *tags, int ntags, const int *units,
                              int nunits),
    void (*learnt_unit)(void *data, int lit, uint32_t tag, const int *units,
                        int nunits),
    void (*conflict)(void *data, const int *lits, int nlits,
                     const uint32_t *tags, int ntags, const int *units,
                     int nunits)) {
  solver->trace_proof_callback_data = data;
  solver->trace_proof_learnt_clause = learnt_clause;
  solver->trace_proof_learnt_unit = learnt_unit;
  solver->trace_proof_conflict = conflict;
  solver->trace_proof = true;
}

void imctk_abc_glucose2_stop_proof_trace(void *s) {
  solver->trace_proof = false;
}

void imctk_abc_glucose2_proof_trace_set_default_tag(void *s, uint32_t tag) {
  solver->trace_default_tag = tag;
}

uint32_t imctk_abc_glucose2_proof_trace_default_tag(void *s) {
  return solver->trace_default_tag;
}

int imctk_abc_glucose2_conflict_size(void *s) {
  return solver->conflict.size();
}

const int *imctk_abc_glucose2_conflict_lits(void *s) {
  return (int *)(Lit *)solver->conflict;
}

ABC_NAMESPACE_IMPL_END
