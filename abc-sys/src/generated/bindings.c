#include "src/bindings.h"

// Static wrappers

int Abc_AbsInt_imctk_abc_sys(int a) { return Abc_AbsInt(a); }
int Abc_MaxInt_imctk_abc_sys(int a, int b) { return Abc_MaxInt(a, b); }
int Abc_MinInt_imctk_abc_sys(int a, int b) { return Abc_MinInt(a, b); }
word Abc_MaxWord_imctk_abc_sys(word a, word b) { return Abc_MaxWord(a, b); }
word Abc_MinWord_imctk_abc_sys(word a, word b) { return Abc_MinWord(a, b); }
float Abc_AbsFloat_imctk_abc_sys(float a) { return Abc_AbsFloat(a); }
float Abc_MaxFloat_imctk_abc_sys(float a, float b) { return Abc_MaxFloat(a, b); }
float Abc_MinFloat_imctk_abc_sys(float a, float b) { return Abc_MinFloat(a, b); }
double Abc_AbsDouble_imctk_abc_sys(double a) { return Abc_AbsDouble(a); }
double Abc_MaxDouble_imctk_abc_sys(double a, double b) { return Abc_MaxDouble(a, b); }
double Abc_MinDouble_imctk_abc_sys(double a, double b) { return Abc_MinDouble(a, b); }
int Abc_Float2Int_imctk_abc_sys(float Val) { return Abc_Float2Int(Val); }
float Abc_Int2Float_imctk_abc_sys(int Num) { return Abc_Int2Float(Num); }
word Abc_Dbl2Word_imctk_abc_sys(double Dbl) { return Abc_Dbl2Word(Dbl); }
double Abc_Word2Dbl_imctk_abc_sys(word Num) { return Abc_Word2Dbl(Num); }
int Abc_Base2Log_imctk_abc_sys(unsigned int n) { return Abc_Base2Log(n); }
int Abc_Base10Log_imctk_abc_sys(unsigned int n) { return Abc_Base10Log(n); }
int Abc_Base16Log_imctk_abc_sys(unsigned int n) { return Abc_Base16Log(n); }
char * Abc_UtilStrsav_imctk_abc_sys(char *s) { return Abc_UtilStrsav(s); }
char * Abc_UtilStrsavTwo_imctk_abc_sys(char *s, char *a) { return Abc_UtilStrsavTwo(s, a); }
char * Abc_UtilStrsavNum_imctk_abc_sys(char *s, int n) { return Abc_UtilStrsavNum(s, n); }
int Abc_BitByteNum_imctk_abc_sys(int nBits) { return Abc_BitByteNum(nBits); }
int Abc_BitWordNum_imctk_abc_sys(int nBits) { return Abc_BitWordNum(nBits); }
int Abc_Bit6WordNum_imctk_abc_sys(int nBits) { return Abc_Bit6WordNum(nBits); }
int Abc_TruthByteNum_imctk_abc_sys(int nVars) { return Abc_TruthByteNum(nVars); }
int Abc_TruthWordNum_imctk_abc_sys(int nVars) { return Abc_TruthWordNum(nVars); }
int Abc_Truth6WordNum_imctk_abc_sys(int nVars) { return Abc_Truth6WordNum(nVars); }
int Abc_InfoHasBit_imctk_abc_sys(unsigned int *p, int i) { return Abc_InfoHasBit(p, i); }
void Abc_InfoSetBit_imctk_abc_sys(unsigned int *p, int i) { Abc_InfoSetBit(p, i); }
void Abc_InfoXorBit_imctk_abc_sys(unsigned int *p, int i) { Abc_InfoXorBit(p, i); }
unsigned int Abc_InfoMask_imctk_abc_sys(int nVar) { return Abc_InfoMask(nVar); }
int Abc_Var2Lit_imctk_abc_sys(int Var, int c) { return Abc_Var2Lit(Var, c); }
int Abc_Lit2Var_imctk_abc_sys(int Lit) { return Abc_Lit2Var(Lit); }
int Abc_LitIsCompl_imctk_abc_sys(int Lit) { return Abc_LitIsCompl(Lit); }
int Abc_LitNot_imctk_abc_sys(int Lit) { return Abc_LitNot(Lit); }
int Abc_LitNotCond_imctk_abc_sys(int Lit, int c) { return Abc_LitNotCond(Lit, c); }
int Abc_LitRegular_imctk_abc_sys(int Lit) { return Abc_LitRegular(Lit); }
int Abc_Lit2LitV_imctk_abc_sys(int *pMap, int Lit) { return Abc_Lit2LitV(pMap, Lit); }
int Abc_Lit2LitL_imctk_abc_sys(int *pMap, int Lit) { return Abc_Lit2LitL(pMap, Lit); }
int Abc_Ptr2Int_imctk_abc_sys(void *p) { return Abc_Ptr2Int(p); }
void * Abc_Int2Ptr_imctk_abc_sys(int i) { return Abc_Int2Ptr(i); }
word Abc_Ptr2Wrd_imctk_abc_sys(void *p) { return Abc_Ptr2Wrd(p); }
void * Abc_Wrd2Ptr_imctk_abc_sys(word i) { return Abc_Wrd2Ptr(i); }
int Abc_Var2Lit2_imctk_abc_sys(int Var, int Att) { return Abc_Var2Lit2(Var, Att); }
int Abc_Lit2Var2_imctk_abc_sys(int Lit) { return Abc_Lit2Var2(Lit); }
int Abc_Lit2Att2_imctk_abc_sys(int Lit) { return Abc_Lit2Att2(Lit); }
int Abc_Var2Lit3_imctk_abc_sys(int Var, int Att) { return Abc_Var2Lit3(Var, Att); }
int Abc_Lit2Var3_imctk_abc_sys(int Lit) { return Abc_Lit2Var3(Lit); }
int Abc_Lit2Att3_imctk_abc_sys(int Lit) { return Abc_Lit2Att3(Lit); }
int Abc_Var2Lit4_imctk_abc_sys(int Var, int Att) { return Abc_Var2Lit4(Var, Att); }
int Abc_Lit2Var4_imctk_abc_sys(int Lit) { return Abc_Lit2Var4(Lit); }
int Abc_Lit2Att4_imctk_abc_sys(int Lit) { return Abc_Lit2Att4(Lit); }
abctime Abc_Clock_imctk_abc_sys(void) { return Abc_Clock(); }
abctime Abc_ThreadClock_imctk_abc_sys(void) { return Abc_ThreadClock(); }
void Abc_PrintInt_imctk_abc_sys(int i) { Abc_PrintInt(i); }
void Abc_PrintTime_imctk_abc_sys(int level, const char *pStr, abctime time) { Abc_PrintTime(level, pStr, time); }
void Abc_PrintTimeP_imctk_abc_sys(int level, const char *pStr, abctime time, abctime Time) { Abc_PrintTimeP(level, pStr, time, Time); }
void Abc_PrintMemoryP_imctk_abc_sys(int level, const char *pStr, int mem, int Mem) { Abc_PrintMemoryP(level, pStr, mem, Mem); }
int Abc_PrimeCudd_imctk_abc_sys(unsigned int p) { return Abc_PrimeCudd(p); }
void * Abc_FileReadContents_imctk_abc_sys(char *pFileName, int *pnFileSize) { return Abc_FileReadContents(pFileName, pnFileSize); }
void Abc_ReverseOrder_imctk_abc_sys(int *pA, int nA) { Abc_ReverseOrder(pA, nA); }
Vec_Int_t * Vec_IntAlloc_imctk_abc_sys(int nCap) { return Vec_IntAlloc(nCap); }
Vec_Int_t * Vec_IntAllocExact_imctk_abc_sys(int nCap) { return Vec_IntAllocExact(nCap); }
Vec_Int_t * Vec_IntStart_imctk_abc_sys(int nSize) { return Vec_IntStart(nSize); }
Vec_Int_t * Vec_IntStartFull_imctk_abc_sys(int nSize) { return Vec_IntStartFull(nSize); }
Vec_Int_t * Vec_IntStartRange_imctk_abc_sys(int First, int Range) { return Vec_IntStartRange(First, Range); }
Vec_Int_t * Vec_IntStartRandomLimit_imctk_abc_sys(int nSize, int Upper, int Lower) { return Vec_IntStartRandomLimit(nSize, Upper, Lower); }
void Vec_IntRandomizeOrder_imctk_abc_sys(Vec_Int_t *p) { Vec_IntRandomizeOrder(p); }
Vec_Int_t * Vec_IntStartNatural_imctk_abc_sys(int nSize) { return Vec_IntStartNatural(nSize); }
Vec_Int_t * Vec_IntAllocArray_imctk_abc_sys(int *pArray, int nSize) { return Vec_IntAllocArray(pArray, nSize); }
Vec_Int_t * Vec_IntAllocArrayCopy_imctk_abc_sys(int *pArray, int nSize) { return Vec_IntAllocArrayCopy(pArray, nSize); }
Vec_Int_t * Vec_IntDup_imctk_abc_sys(Vec_Int_t *pVec) { return Vec_IntDup(pVec); }
Vec_Int_t * Vec_IntDupArray_imctk_abc_sys(Vec_Int_t *pVec) { return Vec_IntDupArray(pVec); }
void Vec_IntZero_imctk_abc_sys(Vec_Int_t *p) { Vec_IntZero(p); }
void Vec_IntErase_imctk_abc_sys(Vec_Int_t *p) { Vec_IntErase(p); }
void Vec_IntFree_imctk_abc_sys(Vec_Int_t *p) { Vec_IntFree(p); }
void Vec_IntFreeP_imctk_abc_sys(Vec_Int_t **p) { Vec_IntFreeP(p); }
int * Vec_IntReleaseArray_imctk_abc_sys(Vec_Int_t *p) { return Vec_IntReleaseArray(p); }
int * Vec_IntReleaseNewArray_imctk_abc_sys(Vec_Int_t *p) { return Vec_IntReleaseNewArray(p); }
int * Vec_IntArray_imctk_abc_sys(Vec_Int_t *p) { return Vec_IntArray(p); }
int ** Vec_IntArrayP_imctk_abc_sys(Vec_Int_t *p) { return Vec_IntArrayP(p); }
int * Vec_IntLimit_imctk_abc_sys(Vec_Int_t *p) { return Vec_IntLimit(p); }
int Vec_IntSize_imctk_abc_sys(Vec_Int_t *p) { return Vec_IntSize(p); }
int Vec_IntCap_imctk_abc_sys(Vec_Int_t *p) { return Vec_IntCap(p); }
double Vec_IntMemory_imctk_abc_sys(Vec_Int_t *p) { return Vec_IntMemory(p); }
int Vec_IntEntry_imctk_abc_sys(Vec_Int_t *p, int i) { return Vec_IntEntry(p, i); }
int * Vec_IntEntryP_imctk_abc_sys(Vec_Int_t *p, int i) { return Vec_IntEntryP(p, i); }
void Vec_IntWriteEntry_imctk_abc_sys(Vec_Int_t *p, int i, int Entry) { Vec_IntWriteEntry(p, i, Entry); }
int Vec_IntAddToEntry_imctk_abc_sys(Vec_Int_t *p, int i, int Addition) { return Vec_IntAddToEntry(p, i, Addition); }
void Vec_IntUpdateEntry_imctk_abc_sys(Vec_Int_t *p, int i, int Value) { Vec_IntUpdateEntry(p, i, Value); }
void Vec_IntDowndateEntry_imctk_abc_sys(Vec_Int_t *p, int i, int Value) { Vec_IntDowndateEntry(p, i, Value); }
int Vec_IntEntryLast_imctk_abc_sys(Vec_Int_t *p) { return Vec_IntEntryLast(p); }
void Vec_IntGrow_imctk_abc_sys(Vec_Int_t *p, int nCapMin) { Vec_IntGrow(p, nCapMin); }
void Vec_IntGrowResize_imctk_abc_sys(Vec_Int_t *p, int nCapMin) { Vec_IntGrowResize(p, nCapMin); }
void Vec_IntFill_imctk_abc_sys(Vec_Int_t *p, int nSize, int Fill) { Vec_IntFill(p, nSize, Fill); }
void Vec_IntFillTwo_imctk_abc_sys(Vec_Int_t *p, int nSize, int FillEven, int FillOdd) { Vec_IntFillTwo(p, nSize, FillEven, FillOdd); }
void Vec_IntFillNatural_imctk_abc_sys(Vec_Int_t *p, int nSize) { Vec_IntFillNatural(p, nSize); }
void Vec_IntFillExtra_imctk_abc_sys(Vec_Int_t *p, int nSize, int Fill) { Vec_IntFillExtra(p, nSize, Fill); }
int Vec_IntGetEntry_imctk_abc_sys(Vec_Int_t *p, int i) { return Vec_IntGetEntry(p, i); }
int Vec_IntGetEntryFull_imctk_abc_sys(Vec_Int_t *p, int i) { return Vec_IntGetEntryFull(p, i); }
int * Vec_IntGetEntryP_imctk_abc_sys(Vec_Int_t *p, int i) { return Vec_IntGetEntryP(p, i); }
void Vec_IntSetEntry_imctk_abc_sys(Vec_Int_t *p, int i, int Entry) { Vec_IntSetEntry(p, i, Entry); }
void Vec_IntSetEntryFull_imctk_abc_sys(Vec_Int_t *p, int i, int Entry) { Vec_IntSetEntryFull(p, i, Entry); }
void Vec_IntShrink_imctk_abc_sys(Vec_Int_t *p, int nSizeNew) { Vec_IntShrink(p, nSizeNew); }
void Vec_IntClear_imctk_abc_sys(Vec_Int_t *p) { Vec_IntClear(p); }
void Vec_IntPush_imctk_abc_sys(Vec_Int_t *p, int Entry) { Vec_IntPush(p, Entry); }
int Vec_IntPushReturn_imctk_abc_sys(Vec_Int_t *p, int Entry) { return Vec_IntPushReturn(p, Entry); }
void Vec_IntPushTwo_imctk_abc_sys(Vec_Int_t *p, int Entry1, int Entry2) { Vec_IntPushTwo(p, Entry1, Entry2); }
void Vec_IntPushThree_imctk_abc_sys(Vec_Int_t *p, int Entry1, int Entry2, int Entry3) { Vec_IntPushThree(p, Entry1, Entry2, Entry3); }
void Vec_IntPushFour_imctk_abc_sys(Vec_Int_t *p, int Entry1, int Entry2, int Entry3, int Entry4) { Vec_IntPushFour(p, Entry1, Entry2, Entry3, Entry4); }
void Vec_IntPushArray_imctk_abc_sys(Vec_Int_t *p, int *pEntries, int nEntries) { Vec_IntPushArray(p, pEntries, nEntries); }
void Vec_IntShift_imctk_abc_sys(Vec_Int_t *p, int Shift) { Vec_IntShift(p, Shift); }
void Vec_IntPushFirst_imctk_abc_sys(Vec_Int_t *p, int Entry) { Vec_IntPushFirst(p, Entry); }
void Vec_IntPushOrder_imctk_abc_sys(Vec_Int_t *p, int Entry) { Vec_IntPushOrder(p, Entry); }
void Vec_IntPushOrderCost_imctk_abc_sys(Vec_Int_t *p, int Entry, Vec_Int_t *vCost) { Vec_IntPushOrderCost(p, Entry, vCost); }
int Vec_IntIsOrdered_imctk_abc_sys(Vec_Int_t *p, int fReverse) { return Vec_IntIsOrdered(p, fReverse); }
int Vec_IntIsOrderedCost_imctk_abc_sys(Vec_Int_t *p, Vec_Int_t *vCost, int fReverse) { return Vec_IntIsOrderedCost(p, vCost, fReverse); }
void Vec_IntPushOrderReverse_imctk_abc_sys(Vec_Int_t *p, int Entry) { Vec_IntPushOrderReverse(p, Entry); }
int Vec_IntPushUniqueOrder_imctk_abc_sys(Vec_Int_t *p, int Entry) { return Vec_IntPushUniqueOrder(p, Entry); }
int Vec_IntPushUniqueOrderCost_imctk_abc_sys(Vec_Int_t *p, int Entry, Vec_Int_t *vCost) { return Vec_IntPushUniqueOrderCost(p, Entry, vCost); }
int Vec_IntPushUnique_imctk_abc_sys(Vec_Int_t *p, int Entry) { return Vec_IntPushUnique(p, Entry); }
unsigned int * Vec_IntFetch_imctk_abc_sys(Vec_Int_t *p, int nWords) { return Vec_IntFetch(p, nWords); }
int Vec_IntPop_imctk_abc_sys(Vec_Int_t *p) { return Vec_IntPop(p); }
int Vec_IntFind_imctk_abc_sys(Vec_Int_t *p, int Entry) { return Vec_IntFind(p, Entry); }
int Vec_IntRemove_imctk_abc_sys(Vec_Int_t *p, int Entry) { return Vec_IntRemove(p, Entry); }
int Vec_IntRemove1_imctk_abc_sys(Vec_Int_t *p, int Entry) { return Vec_IntRemove1(p, Entry); }
void Vec_IntDrop_imctk_abc_sys(Vec_Int_t *p, int i) { Vec_IntDrop(p, i); }
void Vec_IntInsert_imctk_abc_sys(Vec_Int_t *p, int iHere, int Entry) { Vec_IntInsert(p, iHere, Entry); }
int Vec_IntFindMax_imctk_abc_sys(Vec_Int_t *p) { return Vec_IntFindMax(p); }
int Vec_IntArgMax_imctk_abc_sys(Vec_Int_t *p) { return Vec_IntArgMax(p); }
int Vec_IntFindMin_imctk_abc_sys(Vec_Int_t *p) { return Vec_IntFindMin(p); }
int Vec_IntArgMin_imctk_abc_sys(Vec_Int_t *p) { return Vec_IntArgMin(p); }
void Vec_IntReverseOrder_imctk_abc_sys(Vec_Int_t *p) { Vec_IntReverseOrder(p); }
void Vec_IntRemoveOdd_imctk_abc_sys(Vec_Int_t *p) { Vec_IntRemoveOdd(p); }
void Vec_IntRemoveEven_imctk_abc_sys(Vec_Int_t *p) { Vec_IntRemoveEven(p); }
Vec_Int_t * Vec_IntInvert_imctk_abc_sys(Vec_Int_t *p, int Fill) { return Vec_IntInvert(p, Fill); }
Vec_Int_t * Vec_IntCondense_imctk_abc_sys(Vec_Int_t *p, int Fill) { return Vec_IntCondense(p, Fill); }
int Vec_IntSum_imctk_abc_sys(Vec_Int_t *p) { return Vec_IntSum(p); }
int Vec_IntCountEntry_imctk_abc_sys(Vec_Int_t *p, int Entry) { return Vec_IntCountEntry(p, Entry); }
int Vec_IntCountLarger_imctk_abc_sys(Vec_Int_t *p, int Entry) { return Vec_IntCountLarger(p, Entry); }
int Vec_IntCountSmaller_imctk_abc_sys(Vec_Int_t *p, int Entry) { return Vec_IntCountSmaller(p, Entry); }
int Vec_IntCountPositive_imctk_abc_sys(Vec_Int_t *p) { return Vec_IntCountPositive(p); }
int Vec_IntCountZero_imctk_abc_sys(Vec_Int_t *p) { return Vec_IntCountZero(p); }
int Vec_IntAddPositive_imctk_abc_sys(Vec_Int_t *p) { return Vec_IntAddPositive(p); }
int Vec_IntEqual_imctk_abc_sys(Vec_Int_t *p1, Vec_Int_t *p2) { return Vec_IntEqual(p1, p2); }
int Vec_IntContained_imctk_abc_sys(Vec_Int_t *pSmall, Vec_Int_t *pLarge) { return Vec_IntContained(pSmall, pLarge); }
int Vec_IntCountCommon_imctk_abc_sys(Vec_Int_t *p1, Vec_Int_t *p2) { return Vec_IntCountCommon(p1, p2); }
int Vec_IntSortCompare1_imctk_abc_sys(int *pp1, int *pp2) { return Vec_IntSortCompare1(pp1, pp2); }
int Vec_IntSortCompare2_imctk_abc_sys(int *pp1, int *pp2) { return Vec_IntSortCompare2(pp1, pp2); }
void Vec_IntSort_imctk_abc_sys(Vec_Int_t *p, int fReverse) { Vec_IntSort(p, fReverse); }
void Vec_IntSortMulti_imctk_abc_sys(Vec_Int_t *p, int nMulti, int fReverse) { Vec_IntSortMulti(p, nMulti, fReverse); }
int Vec_IntIsSorted_imctk_abc_sys(Vec_Int_t *p, int fReverse) { return Vec_IntIsSorted(p, fReverse); }
int Vec_IntUniqify_imctk_abc_sys(Vec_Int_t *p) { return Vec_IntUniqify(p); }
int Vec_IntCountDuplicates_imctk_abc_sys(Vec_Int_t *p) { return Vec_IntCountDuplicates(p); }
int Vec_IntCheckUniqueSmall_imctk_abc_sys(Vec_Int_t *p) { return Vec_IntCheckUniqueSmall(p); }
int Vec_IntCountUnique_imctk_abc_sys(Vec_Int_t *p) { return Vec_IntCountUnique(p); }
int Vec_IntUniqifyPairs_imctk_abc_sys(Vec_Int_t *p) { return Vec_IntUniqifyPairs(p); }
unsigned int Vec_IntUniqueHashKeyDebug_imctk_abc_sys(unsigned char *pStr, int nChars, int TableMask) { return Vec_IntUniqueHashKeyDebug(pStr, nChars, TableMask); }
void Vec_IntUniqueProfile_imctk_abc_sys(Vec_Int_t *vData, int *pTable, int *pNexts, int TableMask, int nIntSize) { Vec_IntUniqueProfile(vData, pTable, pNexts, TableMask, nIntSize); }
unsigned int Vec_IntUniqueHashKey2_imctk_abc_sys(unsigned char *pStr, int nChars) { return Vec_IntUniqueHashKey2(pStr, nChars); }
unsigned int Vec_IntUniqueHashKey_imctk_abc_sys(unsigned char *pStr, int nChars) { return Vec_IntUniqueHashKey(pStr, nChars); }
int * Vec_IntUniqueLookup_imctk_abc_sys(Vec_Int_t *vData, int i, int nIntSize, int *pNexts, int *pStart) { return Vec_IntUniqueLookup(vData, i, nIntSize, pNexts, pStart); }
int Vec_IntUniqueCount_imctk_abc_sys(Vec_Int_t *vData, int nIntSize, Vec_Int_t **pvMap) { return Vec_IntUniqueCount(vData, nIntSize, pvMap); }
Vec_Int_t * Vec_IntUniqifyHash_imctk_abc_sys(Vec_Int_t *vData, int nIntSize) { return Vec_IntUniqifyHash(vData, nIntSize); }
int Vec_IntSortCompareUnsigned_imctk_abc_sys(unsigned int *pp1, unsigned int *pp2) { return Vec_IntSortCompareUnsigned(pp1, pp2); }
void Vec_IntSortUnsigned_imctk_abc_sys(Vec_Int_t *p) { Vec_IntSortUnsigned(p); }
int Vec_IntTwoCountCommon_imctk_abc_sys(Vec_Int_t *vArr1, Vec_Int_t *vArr2) { return Vec_IntTwoCountCommon(vArr1, vArr2); }
int Vec_IntTwoFindCommon_imctk_abc_sys(Vec_Int_t *vArr1, Vec_Int_t *vArr2, Vec_Int_t *vArr) { return Vec_IntTwoFindCommon(vArr1, vArr2, vArr); }
int Vec_IntTwoFindCommonReverse_imctk_abc_sys(Vec_Int_t *vArr1, Vec_Int_t *vArr2, Vec_Int_t *vArr) { return Vec_IntTwoFindCommonReverse(vArr1, vArr2, vArr); }
int Vec_IntTwoRemoveCommon_imctk_abc_sys(Vec_Int_t *vArr1, Vec_Int_t *vArr2, Vec_Int_t *vArr) { return Vec_IntTwoRemoveCommon(vArr1, vArr2, vArr); }
int Vec_IntTwoRemove_imctk_abc_sys(Vec_Int_t *vArr1, Vec_Int_t *vArr2) { return Vec_IntTwoRemove(vArr1, vArr2); }
void Vec_IntTwoMerge1_imctk_abc_sys(Vec_Int_t *vArr1, Vec_Int_t *vArr2) { Vec_IntTwoMerge1(vArr1, vArr2); }
void Vec_IntTwoRemove1_imctk_abc_sys(Vec_Int_t *vArr1, Vec_Int_t *vArr2) { Vec_IntTwoRemove1(vArr1, vArr2); }
void Vec_IntTwoMerge2Int_imctk_abc_sys(Vec_Int_t *vArr1, Vec_Int_t *vArr2, Vec_Int_t *vArr) { Vec_IntTwoMerge2Int(vArr1, vArr2, vArr); }
Vec_Int_t * Vec_IntTwoMerge_imctk_abc_sys(Vec_Int_t *vArr1, Vec_Int_t *vArr2) { return Vec_IntTwoMerge(vArr1, vArr2); }
void Vec_IntTwoMerge2_imctk_abc_sys(Vec_Int_t *vArr1, Vec_Int_t *vArr2, Vec_Int_t *vArr) { Vec_IntTwoMerge2(vArr1, vArr2, vArr); }
void Vec_IntTwoSplit_imctk_abc_sys(Vec_Int_t *vArr1, Vec_Int_t *vArr2, Vec_Int_t *vArr, Vec_Int_t *vArr1n, Vec_Int_t *vArr2n) { Vec_IntTwoSplit(vArr1, vArr2, vArr, vArr1n, vArr2n); }
void Vec_IntSelectSort_imctk_abc_sys(int *pArray, int nSize) { Vec_IntSelectSort(pArray, nSize); }
void Vec_IntSelectSortReverse_imctk_abc_sys(int *pArray, int nSize) { Vec_IntSelectSortReverse(pArray, nSize); }
void Vec_IntSelectSortCost_imctk_abc_sys(int *pArray, int nSize, Vec_Int_t *vCosts) { Vec_IntSelectSortCost(pArray, nSize, vCosts); }
void Vec_IntSelectSortCostReverse_imctk_abc_sys(int *pArray, int nSize, Vec_Int_t *vCosts) { Vec_IntSelectSortCostReverse(pArray, nSize, vCosts); }
void Vec_IntSelectSortCost2_imctk_abc_sys(int *pArray, int nSize, int *pCosts) { Vec_IntSelectSortCost2(pArray, nSize, pCosts); }
void Vec_IntSelectSortCost2Reverse_imctk_abc_sys(int *pArray, int nSize, int *pCosts) { Vec_IntSelectSortCost2Reverse(pArray, nSize, pCosts); }
void Vec_IntPrint_imctk_abc_sys(Vec_Int_t *vVec) { Vec_IntPrint(vVec); }
void Vec_IntPrintBinary_imctk_abc_sys(Vec_Int_t *vVec) { Vec_IntPrintBinary(vVec); }
int Vec_IntCompareVec_imctk_abc_sys(Vec_Int_t *p1, Vec_Int_t *p2) { return Vec_IntCompareVec(p1, p2); }
void Vec_IntClearAppend_imctk_abc_sys(Vec_Int_t *vVec1, Vec_Int_t *vVec2) { Vec_IntClearAppend(vVec1, vVec2); }
void Vec_IntAppend_imctk_abc_sys(Vec_Int_t *vVec1, Vec_Int_t *vVec2) { Vec_IntAppend(vVec1, vVec2); }
void Vec_IntAppendSkip_imctk_abc_sys(Vec_Int_t *vVec1, Vec_Int_t *vVec2, int iVar) { Vec_IntAppendSkip(vVec1, vVec2, iVar); }
void Vec_IntAppendMinus_imctk_abc_sys(Vec_Int_t *vVec1, Vec_Int_t *vVec2, int fMinus) { Vec_IntAppendMinus(vVec1, vVec2, fMinus); }
void Vec_IntRemapArray_imctk_abc_sys(Vec_Int_t *vOld2New, Vec_Int_t *vOld, Vec_Int_t *vNew, int nNew) { Vec_IntRemapArray(vOld2New, vOld, vNew, nNew); }
void Vec_IntDumpBin_imctk_abc_sys(char *pFileName, Vec_Int_t *p, int fVerbose) { Vec_IntDumpBin(pFileName, p, fVerbose); }
Vec_Int_t * Vec_IntReadBin_imctk_abc_sys(char *pFileName, int fVerbose) { return Vec_IntReadBin(pFileName, fVerbose); }
Vec_Flt_t * Vec_FltAlloc_imctk_abc_sys(int nCap) { return Vec_FltAlloc(nCap); }
Vec_Flt_t * Vec_FltAllocExact_imctk_abc_sys(int nCap) { return Vec_FltAllocExact(nCap); }
Vec_Flt_t * Vec_FltStart_imctk_abc_sys(int nSize) { return Vec_FltStart(nSize); }
Vec_Flt_t * Vec_FltStartFull_imctk_abc_sys(int nSize) { return Vec_FltStartFull(nSize); }
Vec_Flt_t * Vec_FltAllocArray_imctk_abc_sys(float *pArray, int nSize) { return Vec_FltAllocArray(pArray, nSize); }
Vec_Flt_t * Vec_FltAllocArrayCopy_imctk_abc_sys(float *pArray, int nSize) { return Vec_FltAllocArrayCopy(pArray, nSize); }
Vec_Flt_t * Vec_FltDup_imctk_abc_sys(Vec_Flt_t *pVec) { return Vec_FltDup(pVec); }
Vec_Flt_t * Vec_FltDupArray_imctk_abc_sys(Vec_Flt_t *pVec) { return Vec_FltDupArray(pVec); }
void Vec_FltZero_imctk_abc_sys(Vec_Flt_t *p) { Vec_FltZero(p); }
void Vec_FltErase_imctk_abc_sys(Vec_Flt_t *p) { Vec_FltErase(p); }
void Vec_FltFree_imctk_abc_sys(Vec_Flt_t *p) { Vec_FltFree(p); }
void Vec_FltFreeP_imctk_abc_sys(Vec_Flt_t **p) { Vec_FltFreeP(p); }
float * Vec_FltReleaseArray_imctk_abc_sys(Vec_Flt_t *p) { return Vec_FltReleaseArray(p); }
float * Vec_FltArray_imctk_abc_sys(Vec_Flt_t *p) { return Vec_FltArray(p); }
float ** Vec_FltArrayP_imctk_abc_sys(Vec_Flt_t *p) { return Vec_FltArrayP(p); }
int Vec_FltSize_imctk_abc_sys(Vec_Flt_t *p) { return Vec_FltSize(p); }
int Vec_FltCap_imctk_abc_sys(Vec_Flt_t *p) { return Vec_FltCap(p); }
double Vec_FltMemory_imctk_abc_sys(Vec_Flt_t *p) { return Vec_FltMemory(p); }
float Vec_FltEntry_imctk_abc_sys(Vec_Flt_t *p, int i) { return Vec_FltEntry(p, i); }
float * Vec_FltEntryP_imctk_abc_sys(Vec_Flt_t *p, int i) { return Vec_FltEntryP(p, i); }
void Vec_FltWriteEntry_imctk_abc_sys(Vec_Flt_t *p, int i, float Entry) { Vec_FltWriteEntry(p, i, Entry); }
void Vec_FltAddToEntry_imctk_abc_sys(Vec_Flt_t *p, int i, float Addition) { Vec_FltAddToEntry(p, i, Addition); }
void Vec_FltUpdateEntry_imctk_abc_sys(Vec_Flt_t *p, int i, float Value) { Vec_FltUpdateEntry(p, i, Value); }
float Vec_FltEntryLast_imctk_abc_sys(Vec_Flt_t *p) { return Vec_FltEntryLast(p); }
void Vec_FltGrow_imctk_abc_sys(Vec_Flt_t *p, int nCapMin) { Vec_FltGrow(p, nCapMin); }
void Vec_FltFill_imctk_abc_sys(Vec_Flt_t *p, int nSize, float Entry) { Vec_FltFill(p, nSize, Entry); }
void Vec_FltFillExtra_imctk_abc_sys(Vec_Flt_t *p, int nSize, float Fill) { Vec_FltFillExtra(p, nSize, Fill); }
void Vec_FltShrink_imctk_abc_sys(Vec_Flt_t *p, int nSizeNew) { Vec_FltShrink(p, nSizeNew); }
void Vec_FltClear_imctk_abc_sys(Vec_Flt_t *p) { Vec_FltClear(p); }
void Vec_FltPush_imctk_abc_sys(Vec_Flt_t *p, float Entry) { Vec_FltPush(p, Entry); }
void Vec_FltPushOrder_imctk_abc_sys(Vec_Flt_t *p, float Entry) { Vec_FltPushOrder(p, Entry); }
int Vec_FltPushUnique_imctk_abc_sys(Vec_Flt_t *p, float Entry) { return Vec_FltPushUnique(p, Entry); }
float Vec_FltPop_imctk_abc_sys(Vec_Flt_t *p) { return Vec_FltPop(p); }
int Vec_FltFind_imctk_abc_sys(Vec_Flt_t *p, float Entry) { return Vec_FltFind(p, Entry); }
int Vec_FltRemove_imctk_abc_sys(Vec_Flt_t *p, float Entry) { return Vec_FltRemove(p, Entry); }
float Vec_FltFindMax_imctk_abc_sys(Vec_Flt_t *p) { return Vec_FltFindMax(p); }
float Vec_FltFindMin_imctk_abc_sys(Vec_Flt_t *p) { return Vec_FltFindMin(p); }
int Vec_FltEqual_imctk_abc_sys(Vec_Flt_t *p1, Vec_Flt_t *p2) { return Vec_FltEqual(p1, p2); }
void Vec_FltPrint_imctk_abc_sys(Vec_Flt_t *vVec) { Vec_FltPrint(vVec); }
int Vec_FltSortCompare1_imctk_abc_sys(float *pp1, float *pp2) { return Vec_FltSortCompare1(pp1, pp2); }
int Vec_FltSortCompare2_imctk_abc_sys(float *pp1, float *pp2) { return Vec_FltSortCompare2(pp1, pp2); }
void Vec_FltSort_imctk_abc_sys(Vec_Flt_t *p, int fReverse) { Vec_FltSort(p, fReverse); }
Vec_Str_t * Vec_StrAlloc_imctk_abc_sys(int nCap) { return Vec_StrAlloc(nCap); }
Vec_Str_t * Vec_StrAllocExact_imctk_abc_sys(int nCap) { return Vec_StrAllocExact(nCap); }
Vec_Str_t * Vec_StrStart_imctk_abc_sys(int nSize) { return Vec_StrStart(nSize); }
Vec_Str_t * Vec_StrAllocArray_imctk_abc_sys(char *pArray, int nSize) { return Vec_StrAllocArray(pArray, nSize); }
Vec_Str_t * Vec_StrAllocArrayCopy_imctk_abc_sys(char *pArray, int nSize) { return Vec_StrAllocArrayCopy(pArray, nSize); }
Vec_Str_t * Vec_StrDup_imctk_abc_sys(Vec_Str_t *pVec) { return Vec_StrDup(pVec); }
Vec_Str_t * Vec_StrDupArray_imctk_abc_sys(Vec_Str_t *pVec) { return Vec_StrDupArray(pVec); }
void Vec_StrZero_imctk_abc_sys(Vec_Str_t *p) { Vec_StrZero(p); }
void Vec_StrErase_imctk_abc_sys(Vec_Str_t *p) { Vec_StrErase(p); }
void Vec_StrFree_imctk_abc_sys(Vec_Str_t *p) { Vec_StrFree(p); }
void Vec_StrFreeP_imctk_abc_sys(Vec_Str_t **p) { Vec_StrFreeP(p); }
char * Vec_StrReleaseArray_imctk_abc_sys(Vec_Str_t *p) { return Vec_StrReleaseArray(p); }
char * Vec_StrArray_imctk_abc_sys(Vec_Str_t *p) { return Vec_StrArray(p); }
char * Vec_StrLimit_imctk_abc_sys(Vec_Str_t *p) { return Vec_StrLimit(p); }
int Vec_StrSize_imctk_abc_sys(Vec_Str_t *p) { return Vec_StrSize(p); }
void Vec_StrSetSize_imctk_abc_sys(Vec_Str_t *p, int nSize) { Vec_StrSetSize(p, nSize); }
int Vec_StrCap_imctk_abc_sys(Vec_Str_t *p) { return Vec_StrCap(p); }
double Vec_StrMemory_imctk_abc_sys(Vec_Str_t *p) { return Vec_StrMemory(p); }
char Vec_StrEntry_imctk_abc_sys(Vec_Str_t *p, int i) { return Vec_StrEntry(p, i); }
char * Vec_StrEntryP_imctk_abc_sys(Vec_Str_t *p, int i) { return Vec_StrEntryP(p, i); }
void Vec_StrWriteEntry_imctk_abc_sys(Vec_Str_t *p, int i, char Entry) { Vec_StrWriteEntry(p, i, Entry); }
char Vec_StrEntryLast_imctk_abc_sys(Vec_Str_t *p) { return Vec_StrEntryLast(p); }
void Vec_StrGrow_imctk_abc_sys(Vec_Str_t *p, int nCapMin) { Vec_StrGrow(p, nCapMin); }
void Vec_StrFill_imctk_abc_sys(Vec_Str_t *p, int nSize, char Fill) { Vec_StrFill(p, nSize, Fill); }
void Vec_StrFillExtra_imctk_abc_sys(Vec_Str_t *p, int nSize, char Fill) { Vec_StrFillExtra(p, nSize, Fill); }
char Vec_StrGetEntry_imctk_abc_sys(Vec_Str_t *p, int i) { return Vec_StrGetEntry(p, i); }
void Vec_StrSetEntry_imctk_abc_sys(Vec_Str_t *p, int i, char Entry) { Vec_StrSetEntry(p, i, Entry); }
void Vec_StrShrink_imctk_abc_sys(Vec_Str_t *p, int nSizeNew) { Vec_StrShrink(p, nSizeNew); }
void Vec_StrClear_imctk_abc_sys(Vec_Str_t *p) { Vec_StrClear(p); }
void Vec_StrPush_imctk_abc_sys(Vec_Str_t *p, char Entry) { Vec_StrPush(p, Entry); }
void Vec_StrPushTwo_imctk_abc_sys(Vec_Str_t *p, char Entry1, char Entry2) { Vec_StrPushTwo(p, Entry1, Entry2); }
void Vec_StrPushBuffer_imctk_abc_sys(Vec_Str_t *p, char *pBuffer, int nSize) { Vec_StrPushBuffer(p, pBuffer, nSize); }
char Vec_StrPop_imctk_abc_sys(Vec_Str_t *p) { return Vec_StrPop(p); }
void Vec_StrIntPrint_imctk_abc_sys(Vec_Str_t *p) { Vec_StrIntPrint(p); }
void Vec_StrPrintNum_imctk_abc_sys(Vec_Str_t *p, int Num) { Vec_StrPrintNum(p, Num); }
void Vec_StrPrintNumStar_imctk_abc_sys(Vec_Str_t *p, int Num, int nDigits) { Vec_StrPrintNumStar(p, Num, nDigits); }
void Vec_StrPrintStr_imctk_abc_sys(Vec_Str_t *p, const char *pStr) { Vec_StrPrintStr(p, pStr); }
void Vec_StrAppend_imctk_abc_sys(Vec_Str_t *p, const char *pString) { Vec_StrAppend(p, pString); }
void Vec_StrCopy_imctk_abc_sys(Vec_Str_t *p, const char *pString) { Vec_StrCopy(p, pString); }
void Vec_StrReverseOrder_imctk_abc_sys(Vec_Str_t *p) { Vec_StrReverseOrder(p); }
int Vec_StrSum_imctk_abc_sys(Vec_Str_t *p) { return Vec_StrSum(p); }
int Vec_StrCountEntry_imctk_abc_sys(Vec_Str_t *p, char Entry) { return Vec_StrCountEntry(p, Entry); }
int Vec_StrCountLarger_imctk_abc_sys(Vec_Str_t *p, char Entry) { return Vec_StrCountLarger(p, Entry); }
int Vec_StrCountSmaller_imctk_abc_sys(Vec_Str_t *p, char Entry) { return Vec_StrCountSmaller(p, Entry); }
int Vec_StrCountEntryLit_imctk_abc_sys(Vec_Str_t *p, char Entry) { return Vec_StrCountEntryLit(p, Entry); }
int Vec_StrCountLargerLit_imctk_abc_sys(Vec_Str_t *p, char Entry) { return Vec_StrCountLargerLit(p, Entry); }
int Vec_StrCountSmallerLit_imctk_abc_sys(Vec_Str_t *p, char Entry) { return Vec_StrCountSmallerLit(p, Entry); }
int Vec_StrEqual_imctk_abc_sys(Vec_Str_t *p1, Vec_Str_t *p2) { return Vec_StrEqual(p1, p2); }
int Vec_StrSortCompare1_imctk_abc_sys(char *pp1, char *pp2) { return Vec_StrSortCompare1(pp1, pp2); }
int Vec_StrSortCompare2_imctk_abc_sys(char *pp1, char *pp2) { return Vec_StrSortCompare2(pp1, pp2); }
void Vec_StrSort_imctk_abc_sys(Vec_Str_t *p, int fReverse) { Vec_StrSort(p, fReverse); }
int Vec_StrCompareVec_imctk_abc_sys(Vec_Str_t *p1, Vec_Str_t *p2) { return Vec_StrCompareVec(p1, p2); }
void Vec_StrPutI_ne_imctk_abc_sys(Vec_Str_t *vOut, int Val) { Vec_StrPutI_ne(vOut, Val); }
int Vec_StrGetI_ne_imctk_abc_sys(Vec_Str_t *vOut, int *pPos) { return Vec_StrGetI_ne(vOut, pPos); }
void Vec_StrPutI_imctk_abc_sys(Vec_Str_t *vOut, int Val) { Vec_StrPutI(vOut, Val); }
int Vec_StrGetI_imctk_abc_sys(Vec_Str_t *vOut, int *pPos) { return Vec_StrGetI(vOut, pPos); }
void Vec_StrPutW_imctk_abc_sys(Vec_Str_t *vOut, word Val) { Vec_StrPutW(vOut, Val); }
word Vec_StrGetW_imctk_abc_sys(Vec_Str_t *vOut, int *pPos) { return Vec_StrGetW(vOut, pPos); }
void Vec_StrPutF_imctk_abc_sys(Vec_Str_t *vOut, float Val) { Vec_StrPutF(vOut, Val); }
float Vec_StrGetF_imctk_abc_sys(Vec_Str_t *vOut, int *pPos) { return Vec_StrGetF(vOut, pPos); }
void Vec_StrPutD_imctk_abc_sys(Vec_Str_t *vOut, double Val) { Vec_StrPutD(vOut, Val); }
double Vec_StrGetD_imctk_abc_sys(Vec_Str_t *vOut, int *pPos) { return Vec_StrGetD(vOut, pPos); }
void Vec_StrPutS_imctk_abc_sys(Vec_Str_t *vOut, char *pStr) { Vec_StrPutS(vOut, pStr); }
char * Vec_StrGetS_imctk_abc_sys(Vec_Str_t *vOut, int *pPos) { return Vec_StrGetS(vOut, pPos); }
void Vec_StrPutC_imctk_abc_sys(Vec_Str_t *vOut, char c) { Vec_StrPutC(vOut, c); }
char Vec_StrGetC_imctk_abc_sys(Vec_Str_t *vOut, int *pPos) { return Vec_StrGetC(vOut, pPos); }
Vec_Ptr_t * Vec_PtrAlloc_imctk_abc_sys(int nCap) { return Vec_PtrAlloc(nCap); }
Vec_Ptr_t * Vec_PtrAllocExact_imctk_abc_sys(int nCap) { return Vec_PtrAllocExact(nCap); }
Vec_Ptr_t * Vec_PtrStart_imctk_abc_sys(int nSize) { return Vec_PtrStart(nSize); }
Vec_Ptr_t * Vec_PtrAllocArray_imctk_abc_sys(void **pArray, int nSize) { return Vec_PtrAllocArray(pArray, nSize); }
Vec_Ptr_t * Vec_PtrAllocArrayCopy_imctk_abc_sys(void **pArray, int nSize) { return Vec_PtrAllocArrayCopy(pArray, nSize); }
Vec_Ptr_t * Vec_PtrDup_imctk_abc_sys(Vec_Ptr_t *pVec) { return Vec_PtrDup(pVec); }
Vec_Ptr_t * Vec_PtrDupStr_imctk_abc_sys(Vec_Ptr_t *pVec) { return Vec_PtrDupStr(pVec); }
Vec_Ptr_t * Vec_PtrDupArray_imctk_abc_sys(Vec_Ptr_t *pVec) { return Vec_PtrDupArray(pVec); }
void Vec_PtrZero_imctk_abc_sys(Vec_Ptr_t *p) { Vec_PtrZero(p); }
void Vec_PtrErase_imctk_abc_sys(Vec_Ptr_t *p) { Vec_PtrErase(p); }
void Vec_PtrFree_imctk_abc_sys(Vec_Ptr_t *p) { Vec_PtrFree(p); }
void Vec_PtrFreeP_imctk_abc_sys(Vec_Ptr_t **p) { Vec_PtrFreeP(p); }
void ** Vec_PtrReleaseArray_imctk_abc_sys(Vec_Ptr_t *p) { return Vec_PtrReleaseArray(p); }
void ** Vec_PtrArray_imctk_abc_sys(Vec_Ptr_t *p) { return Vec_PtrArray(p); }
int Vec_PtrSize_imctk_abc_sys(Vec_Ptr_t *p) { return Vec_PtrSize(p); }
int Vec_PtrCap_imctk_abc_sys(Vec_Ptr_t *p) { return Vec_PtrCap(p); }
double Vec_PtrMemory_imctk_abc_sys(Vec_Ptr_t *p) { return Vec_PtrMemory(p); }
int Vec_PtrCountZero_imctk_abc_sys(Vec_Ptr_t *p) { return Vec_PtrCountZero(p); }
void * Vec_PtrEntry_imctk_abc_sys(Vec_Ptr_t *p, int i) { return Vec_PtrEntry(p, i); }
void ** Vec_PtrEntryP_imctk_abc_sys(Vec_Ptr_t *p, int i) { return Vec_PtrEntryP(p, i); }
void Vec_PtrWriteEntry_imctk_abc_sys(Vec_Ptr_t *p, int i, void *Entry) { Vec_PtrWriteEntry(p, i, Entry); }
void * Vec_PtrEntryLast_imctk_abc_sys(Vec_Ptr_t *p) { return Vec_PtrEntryLast(p); }
void Vec_PtrGrow_imctk_abc_sys(Vec_Ptr_t *p, int nCapMin) { Vec_PtrGrow(p, nCapMin); }
void Vec_PtrFill_imctk_abc_sys(Vec_Ptr_t *p, int nSize, void *Entry) { Vec_PtrFill(p, nSize, Entry); }
void Vec_PtrFillTwo_imctk_abc_sys(Vec_Ptr_t *p, int nSize, void *EntryEven, void *EntryOdd) { Vec_PtrFillTwo(p, nSize, EntryEven, EntryOdd); }
void Vec_PtrFillExtra_imctk_abc_sys(Vec_Ptr_t *p, int nSize, void *Fill) { Vec_PtrFillExtra(p, nSize, Fill); }
void * Vec_PtrGetEntry_imctk_abc_sys(Vec_Ptr_t *p, int i) { return Vec_PtrGetEntry(p, i); }
void Vec_PtrSetEntry_imctk_abc_sys(Vec_Ptr_t *p, int i, void *Entry) { Vec_PtrSetEntry(p, i, Entry); }
void Vec_PtrShrink_imctk_abc_sys(Vec_Ptr_t *p, int nSizeNew) { Vec_PtrShrink(p, nSizeNew); }
void Vec_PtrClear_imctk_abc_sys(Vec_Ptr_t *p) { Vec_PtrClear(p); }
void Vec_PtrFreeData_imctk_abc_sys(Vec_Ptr_t *p) { Vec_PtrFreeData(p); }
void Vec_PtrFreeFree_imctk_abc_sys(Vec_Ptr_t *p) { Vec_PtrFreeFree(p); }
void Vec_PtrFreeFunc_imctk_abc_sys(Vec_Ptr_t *p, void (*pFuncItemFree) (void *)) { Vec_PtrFreeFunc(p, pFuncItemFree); }
void Vec_PtrCopy_imctk_abc_sys(Vec_Ptr_t *pDest, Vec_Ptr_t *pSour) { Vec_PtrCopy(pDest, pSour); }
void Vec_PtrPrintNames_imctk_abc_sys(Vec_Ptr_t *p) { Vec_PtrPrintNames(p); }
void Vec_PtrPush_imctk_abc_sys(Vec_Ptr_t *p, void *Entry) { Vec_PtrPush(p, Entry); }
void Vec_PtrPushTwo_imctk_abc_sys(Vec_Ptr_t *p, void *Entry1, void *Entry2) { Vec_PtrPushTwo(p, Entry1, Entry2); }
void Vec_PtrAppend_imctk_abc_sys(Vec_Ptr_t *vVec1, Vec_Ptr_t *vVec2) { Vec_PtrAppend(vVec1, vVec2); }
void Vec_PtrPushFirst_imctk_abc_sys(Vec_Ptr_t *p, void *Entry) { Vec_PtrPushFirst(p, Entry); }
int Vec_PtrPushUnique_imctk_abc_sys(Vec_Ptr_t *p, void *Entry) { return Vec_PtrPushUnique(p, Entry); }
void * Vec_PtrPop_imctk_abc_sys(Vec_Ptr_t *p) { return Vec_PtrPop(p); }
int Vec_PtrFind_imctk_abc_sys(Vec_Ptr_t *p, void *Entry) { return Vec_PtrFind(p, Entry); }
int Vec_PtrFindStr_imctk_abc_sys(Vec_Ptr_t *p, char *Entry) { return Vec_PtrFindStr(p, Entry); }
void Vec_PtrRemove_imctk_abc_sys(Vec_Ptr_t *p, void *Entry) { Vec_PtrRemove(p, Entry); }
void Vec_PtrDrop_imctk_abc_sys(Vec_Ptr_t *p, int i) { Vec_PtrDrop(p, i); }
void Vec_PtrInsert_imctk_abc_sys(Vec_Ptr_t *p, int iHere, void *Entry) { Vec_PtrInsert(p, iHere, Entry); }
void Vec_PtrReorder_imctk_abc_sys(Vec_Ptr_t *p, int nItems) { Vec_PtrReorder(p, nItems); }
void Vec_PtrReverseOrder_imctk_abc_sys(Vec_Ptr_t *p) { Vec_PtrReverseOrder(p); }
int Vec_PtrEqual_imctk_abc_sys(Vec_Ptr_t *p1, Vec_Ptr_t *p2) { return Vec_PtrEqual(p1, p2); }
int Vec_PtrSortComparePtr_imctk_abc_sys(void **pp1, void **pp2) { return Vec_PtrSortComparePtr(pp1, pp2); }
void Vec_PtrSort_imctk_abc_sys(Vec_Ptr_t *p, int (*Vec_PtrSortCompare) (const void *, const void *)) { Vec_PtrSort(p, Vec_PtrSortCompare); }
void Vec_PtrUniqify_imctk_abc_sys(Vec_Ptr_t *p, int (*Vec_PtrSortCompare) (const void *, const void *)) { Vec_PtrUniqify(p, Vec_PtrSortCompare); }
void Vec_PtrUniqify2_imctk_abc_sys(Vec_Ptr_t *p, int (*Vec_PtrSortCompare) (const void *, const void *), void (*Vec_PtrObjFree) (void *), Vec_Int_t *vCounts) { Vec_PtrUniqify2(p, Vec_PtrSortCompare, Vec_PtrObjFree, vCounts); }
Vec_Ptr_t * Vec_PtrAllocSimInfo_imctk_abc_sys(int nEntries, int nWords) { return Vec_PtrAllocSimInfo(nEntries, nWords); }
int Vec_PtrReadWordsSimInfo_imctk_abc_sys(Vec_Ptr_t *p) { return Vec_PtrReadWordsSimInfo(p); }
void Vec_PtrCleanSimInfo_imctk_abc_sys(Vec_Ptr_t *vInfo, int iWord, int nWords) { Vec_PtrCleanSimInfo(vInfo, iWord, nWords); }
void Vec_PtrFillSimInfo_imctk_abc_sys(Vec_Ptr_t *vInfo, int iWord, int nWords) { Vec_PtrFillSimInfo(vInfo, iWord, nWords); }
void Vec_PtrDoubleSimInfo_imctk_abc_sys(Vec_Ptr_t *vInfo) { Vec_PtrDoubleSimInfo(vInfo); }
void Vec_PtrReallocSimInfo_imctk_abc_sys(Vec_Ptr_t *vInfo) { Vec_PtrReallocSimInfo(vInfo); }
Vec_Ptr_t * Vec_PtrAllocTruthTables_imctk_abc_sys(int nVars) { return Vec_PtrAllocTruthTables(nVars); }
Vec_Vec_t * Vec_VecAlloc_imctk_abc_sys(int nCap) { return Vec_VecAlloc(nCap); }
Vec_Vec_t * Vec_VecStart_imctk_abc_sys(int nSize) { return Vec_VecStart(nSize); }
void Vec_VecExpand_imctk_abc_sys(Vec_Vec_t *p, int Level) { Vec_VecExpand(p, Level); }
void Vec_VecExpandInt_imctk_abc_sys(Vec_Vec_t *p, int Level) { Vec_VecExpandInt(p, Level); }
int Vec_VecSize_imctk_abc_sys(Vec_Vec_t *p) { return Vec_VecSize(p); }
int Vec_VecCap_imctk_abc_sys(Vec_Vec_t *p) { return Vec_VecCap(p); }
int Vec_VecLevelSize_imctk_abc_sys(Vec_Vec_t *p, int i) { return Vec_VecLevelSize(p, i); }
Vec_Ptr_t * Vec_VecEntry_imctk_abc_sys(Vec_Vec_t *p, int i) { return Vec_VecEntry(p, i); }
Vec_Int_t * Vec_VecEntryInt_imctk_abc_sys(Vec_Vec_t *p, int i) { return Vec_VecEntryInt(p, i); }
double Vec_VecMemory_imctk_abc_sys(Vec_Vec_t *p) { return Vec_VecMemory(p); }
double Vec_VecMemoryInt_imctk_abc_sys(Vec_Vec_t *p) { return Vec_VecMemoryInt(p); }
void * Vec_VecEntryEntry_imctk_abc_sys(Vec_Vec_t *p, int i, int k) { return Vec_VecEntryEntry(p, i, k); }
int Vec_VecEntryEntryInt_imctk_abc_sys(Vec_Vec_t *p, int i, int k) { return Vec_VecEntryEntryInt(p, i, k); }
void Vec_VecFree_imctk_abc_sys(Vec_Vec_t *p) { Vec_VecFree(p); }
void Vec_VecErase_imctk_abc_sys(Vec_Vec_t *p) { Vec_VecErase(p); }
void Vec_VecFreeP_imctk_abc_sys(Vec_Vec_t **p) { Vec_VecFreeP(p); }
Vec_Vec_t * Vec_VecDup_imctk_abc_sys(Vec_Vec_t *p) { return Vec_VecDup(p); }
Vec_Vec_t * Vec_VecDupInt_imctk_abc_sys(Vec_Vec_t *p) { return Vec_VecDupInt(p); }
int Vec_VecSizeSize_imctk_abc_sys(Vec_Vec_t *p) { return Vec_VecSizeSize(p); }
void Vec_VecClear_imctk_abc_sys(Vec_Vec_t *p) { Vec_VecClear(p); }
void Vec_VecPush_imctk_abc_sys(Vec_Vec_t *p, int Level, void *Entry) { Vec_VecPush(p, Level, Entry); }
void Vec_VecPushInt_imctk_abc_sys(Vec_Vec_t *p, int Level, int Entry) { Vec_VecPushInt(p, Level, Entry); }
void Vec_VecPushUnique_imctk_abc_sys(Vec_Vec_t *p, int Level, void *Entry) { Vec_VecPushUnique(p, Level, Entry); }
void Vec_VecPushUniqueInt_imctk_abc_sys(Vec_Vec_t *p, int Level, int Entry) { Vec_VecPushUniqueInt(p, Level, Entry); }
int Vec_VecSortCompare1_imctk_abc_sys(Vec_Ptr_t **pp1, Vec_Ptr_t **pp2) { return Vec_VecSortCompare1(pp1, pp2); }
int Vec_VecSortCompare2_imctk_abc_sys(Vec_Ptr_t **pp1, Vec_Ptr_t **pp2) { return Vec_VecSortCompare2(pp1, pp2); }
void Vec_VecSort_imctk_abc_sys(Vec_Vec_t *p, int fReverse) { Vec_VecSort(p, fReverse); }
int Vec_VecSortCompare3_imctk_abc_sys(Vec_Int_t **pp1, Vec_Int_t **pp2) { return Vec_VecSortCompare3(pp1, pp2); }
int Vec_VecSortCompare4_imctk_abc_sys(Vec_Int_t **pp1, Vec_Int_t **pp2) { return Vec_VecSortCompare4(pp1, pp2); }
void Vec_VecSortByFirstInt_imctk_abc_sys(Vec_Vec_t *p, int fReverse) { Vec_VecSortByFirstInt(p, fReverse); }
void Vec_VecPrintInt_imctk_abc_sys(Vec_Vec_t *p, int fSkipSingles) { Vec_VecPrintInt(p, fSkipSingles); }
Vec_Att_t * Vec_AttAlloc_imctk_abc_sys(int nSize, void *pMan, void (*pFuncFreeMan) (void *), void * (*pFuncStartObj) (void *), void (*pFuncFreeObj) (void *, void *)) { return Vec_AttAlloc(nSize, pMan, pFuncFreeMan, pFuncStartObj, pFuncFreeObj); }
void * Vec_AttFree_imctk_abc_sys(Vec_Att_t *p, int fFreeMan) { return Vec_AttFree(p, fFreeMan); }
void Vec_AttClear_imctk_abc_sys(Vec_Att_t *p) { Vec_AttClear(p); }
void Vec_AttFreeEntry_imctk_abc_sys(Vec_Att_t *p, int i) { Vec_AttFreeEntry(p, i); }
void Vec_AttGrow_imctk_abc_sys(Vec_Att_t *p, int nCapMin) { Vec_AttGrow(p, nCapMin); }
void Vec_AttWriteEntry_imctk_abc_sys(Vec_Att_t *p, int i, void *pEntry) { Vec_AttWriteEntry(p, i, pEntry); }
void * Vec_AttEntry_imctk_abc_sys(Vec_Att_t *p, int i) { return Vec_AttEntry(p, i); }
void * Vec_AttMan_imctk_abc_sys(Vec_Att_t *p) { return Vec_AttMan(p); }
void ** Vec_AttArray_imctk_abc_sys(Vec_Att_t *p) { return Vec_AttArray(p); }
Vec_Wrd_t * Vec_WrdAlloc_imctk_abc_sys(int nCap) { return Vec_WrdAlloc(nCap); }
Vec_Wrd_t * Vec_WrdAllocExact_imctk_abc_sys(int nCap) { return Vec_WrdAllocExact(nCap); }
Vec_Wrd_t * Vec_WrdStart_imctk_abc_sys(int nSize) { return Vec_WrdStart(nSize); }
Vec_Wrd_t * Vec_WrdStartFull_imctk_abc_sys(int nSize) { return Vec_WrdStartFull(nSize); }
Vec_Wrd_t * Vec_WrdStartNatural_imctk_abc_sys(int nSize) { return Vec_WrdStartNatural(nSize); }
Vec_Wrd_t * Vec_WrdStartRandom_imctk_abc_sys(int nSize) { return Vec_WrdStartRandom(nSize); }
Vec_Wrd_t * Vec_WrdStartTruthTables_imctk_abc_sys(int nVars) { return Vec_WrdStartTruthTables(nVars); }
Vec_Wrd_t * Vec_WrdStartTruthTablesRev_imctk_abc_sys(int nVars) { return Vec_WrdStartTruthTablesRev(nVars); }
int Vec_WrdShiftOne_imctk_abc_sys(Vec_Wrd_t *p, int nWords) { return Vec_WrdShiftOne(p, nWords); }
Vec_Wrd_t * Vec_WrdAllocArray_imctk_abc_sys(word *pArray, int nSize) { return Vec_WrdAllocArray(pArray, nSize); }
Vec_Wrd_t * Vec_WrdAllocArrayCopy_imctk_abc_sys(word *pArray, int nSize) { return Vec_WrdAllocArrayCopy(pArray, nSize); }
Vec_Wrd_t * Vec_WrdDup_imctk_abc_sys(Vec_Wrd_t *pVec) { return Vec_WrdDup(pVec); }
Vec_Wrd_t * Vec_WrdDupArray_imctk_abc_sys(Vec_Wrd_t *pVec) { return Vec_WrdDupArray(pVec); }
void Vec_WrdErase_imctk_abc_sys(Vec_Wrd_t *p) { Vec_WrdErase(p); }
void Vec_WrdFree_imctk_abc_sys(Vec_Wrd_t *p) { Vec_WrdFree(p); }
void Vec_WrdFreeP_imctk_abc_sys(Vec_Wrd_t **p) { Vec_WrdFreeP(p); }
word * Vec_WrdReleaseArray_imctk_abc_sys(Vec_Wrd_t *p) { return Vec_WrdReleaseArray(p); }
word * Vec_WrdArray_imctk_abc_sys(Vec_Wrd_t *p) { return Vec_WrdArray(p); }
word * Vec_WrdLimit_imctk_abc_sys(Vec_Wrd_t *p) { return Vec_WrdLimit(p); }
int Vec_WrdSize_imctk_abc_sys(Vec_Wrd_t *p) { return Vec_WrdSize(p); }
int Vec_WrdChangeSize_imctk_abc_sys(Vec_Wrd_t *p, int Shift) { return Vec_WrdChangeSize(p, Shift); }
int Vec_WrdCap_imctk_abc_sys(Vec_Wrd_t *p) { return Vec_WrdCap(p); }
double Vec_WrdMemory_imctk_abc_sys(Vec_Wrd_t *p) { return Vec_WrdMemory(p); }
word Vec_WrdEntry_imctk_abc_sys(Vec_Wrd_t *p, int i) { return Vec_WrdEntry(p, i); }
word * Vec_WrdEntryP_imctk_abc_sys(Vec_Wrd_t *p, int i) { return Vec_WrdEntryP(p, i); }
void Vec_WrdWriteEntry_imctk_abc_sys(Vec_Wrd_t *p, int i, word Entry) { Vec_WrdWriteEntry(p, i, Entry); }
word Vec_WrdAddToEntry_imctk_abc_sys(Vec_Wrd_t *p, int i, word Addition) { return Vec_WrdAddToEntry(p, i, Addition); }
word Vec_WrdEntryLast_imctk_abc_sys(Vec_Wrd_t *p) { return Vec_WrdEntryLast(p); }
void Vec_WrdGrow_imctk_abc_sys(Vec_Wrd_t *p, int nCapMin) { Vec_WrdGrow(p, nCapMin); }
void Vec_WrdFill_imctk_abc_sys(Vec_Wrd_t *p, int nSize, word Fill) { Vec_WrdFill(p, nSize, Fill); }
void Vec_WrdFillExtra_imctk_abc_sys(Vec_Wrd_t *p, int nSize, word Fill) { Vec_WrdFillExtra(p, nSize, Fill); }
word Vec_WrdGetEntry_imctk_abc_sys(Vec_Wrd_t *p, int i) { return Vec_WrdGetEntry(p, i); }
word * Vec_WrdGetEntryP_imctk_abc_sys(Vec_Wrd_t *p, int i) { return Vec_WrdGetEntryP(p, i); }
void Vec_WrdSetEntry_imctk_abc_sys(Vec_Wrd_t *p, int i, word Entry) { Vec_WrdSetEntry(p, i, Entry); }
void Vec_WrdShrink_imctk_abc_sys(Vec_Wrd_t *p, int nSizeNew) { Vec_WrdShrink(p, nSizeNew); }
void Vec_WrdClear_imctk_abc_sys(Vec_Wrd_t *p) { Vec_WrdClear(p); }
void Vec_WrdPush_imctk_abc_sys(Vec_Wrd_t *p, word Entry) { Vec_WrdPush(p, Entry); }
void Vec_WrdPushTwo_imctk_abc_sys(Vec_Wrd_t *p, word Entry1, word Entry2) { Vec_WrdPushTwo(p, Entry1, Entry2); }
void Vec_WrdPushThree_imctk_abc_sys(Vec_Wrd_t *p, word Entry1, word Entry2, word Entry3) { Vec_WrdPushThree(p, Entry1, Entry2, Entry3); }
void Vec_WrdPushFour_imctk_abc_sys(Vec_Wrd_t *p, word Entry1, word Entry2, word Entry3, word Entry4) { Vec_WrdPushFour(p, Entry1, Entry2, Entry3, Entry4); }
void Vec_WrdPushArray_imctk_abc_sys(Vec_Wrd_t *p, word *pEntries, int nEntries) { Vec_WrdPushArray(p, pEntries, nEntries); }
void Vec_WrdPushFirst_imctk_abc_sys(Vec_Wrd_t *p, word Entry) { Vec_WrdPushFirst(p, Entry); }
void Vec_WrdPushOrder_imctk_abc_sys(Vec_Wrd_t *p, word Entry) { Vec_WrdPushOrder(p, Entry); }
int Vec_WrdPushUniqueOrder_imctk_abc_sys(Vec_Wrd_t *p, word Entry) { return Vec_WrdPushUniqueOrder(p, Entry); }
int Vec_WrdPushUnique_imctk_abc_sys(Vec_Wrd_t *p, word Entry) { return Vec_WrdPushUnique(p, Entry); }
word * Vec_WrdFetch_imctk_abc_sys(Vec_Wrd_t *p, int nWords) { return Vec_WrdFetch(p, nWords); }
word Vec_WrdPop_imctk_abc_sys(Vec_Wrd_t *p) { return Vec_WrdPop(p); }
int Vec_WrdFind_imctk_abc_sys(Vec_Wrd_t *p, word Entry) { return Vec_WrdFind(p, Entry); }
int Vec_WrdRemove_imctk_abc_sys(Vec_Wrd_t *p, word Entry) { return Vec_WrdRemove(p, Entry); }
void Vec_WrdInsert_imctk_abc_sys(Vec_Wrd_t *p, int iHere, word Entry) { Vec_WrdInsert(p, iHere, Entry); }
void Vec_WrdDrop_imctk_abc_sys(Vec_Wrd_t *p, int i) { Vec_WrdDrop(p, i); }
word Vec_WrdFindMax_imctk_abc_sys(Vec_Wrd_t *p) { return Vec_WrdFindMax(p); }
word Vec_WrdFindMin_imctk_abc_sys(Vec_Wrd_t *p) { return Vec_WrdFindMin(p); }
void Vec_WrdReverseOrder_imctk_abc_sys(Vec_Wrd_t *p) { Vec_WrdReverseOrder(p); }
Vec_Wrd_t * Vec_WrdInvert_imctk_abc_sys(Vec_Wrd_t *p, word Fill) { return Vec_WrdInvert(p, Fill); }
word Vec_WrdSum_imctk_abc_sys(Vec_Wrd_t *p) { return Vec_WrdSum(p); }
int Vec_WrdCountZero_imctk_abc_sys(Vec_Wrd_t *p) { return Vec_WrdCountZero(p); }
int Vec_WrdEqual_imctk_abc_sys(Vec_Wrd_t *p1, Vec_Wrd_t *p2) { return Vec_WrdEqual(p1, p2); }
int Vec_WrdCountCommon_imctk_abc_sys(Vec_Wrd_t *p1, Vec_Wrd_t *p2) { return Vec_WrdCountCommon(p1, p2); }
int Vec_WrdSortCompare1_imctk_abc_sys(word *pp1, word *pp2) { return Vec_WrdSortCompare1(pp1, pp2); }
int Vec_WrdSortCompare2_imctk_abc_sys(word *pp1, word *pp2) { return Vec_WrdSortCompare2(pp1, pp2); }
void Vec_WrdSort_imctk_abc_sys(Vec_Wrd_t *p, int fReverse) { Vec_WrdSort(p, fReverse); }
void Vec_WrdUniqify_imctk_abc_sys(Vec_Wrd_t *p) { Vec_WrdUniqify(p); }
int Vec_WrdUniqueCount_imctk_abc_sys(Vec_Wrd_t *vData, int nWordSize, Vec_Int_t **pvMap) { return Vec_WrdUniqueCount(vData, nWordSize, pvMap); }
Vec_Wrd_t * Vec_WrdUniqifyHash_imctk_abc_sys(Vec_Wrd_t *vData, int nWordSize) { return Vec_WrdUniqifyHash(vData, nWordSize); }
int Vec_WrdTwoCountCommon_imctk_abc_sys(Vec_Wrd_t *vArr1, Vec_Wrd_t *vArr2) { return Vec_WrdTwoCountCommon(vArr1, vArr2); }
int Vec_WrdSortCompareUnsigned_imctk_abc_sys(word *pp1, word *pp2) { return Vec_WrdSortCompareUnsigned(pp1, pp2); }
void Vec_WrdSortUnsigned_imctk_abc_sys(Vec_Wrd_t *p) { Vec_WrdSortUnsigned(p); }
void Vec_WrdAppend_imctk_abc_sys(Vec_Wrd_t *vVec1, Vec_Wrd_t *vVec2) { Vec_WrdAppend(vVec1, vVec2); }
void Vec_WrdDumpBoolOne_imctk_abc_sys(FILE *pFile, word *pSim, int nBits, int fReverse) { Vec_WrdDumpBoolOne(pFile, pSim, nBits, fReverse); }
void Vec_WrdDumpBool_imctk_abc_sys(char *pFileName, Vec_Wrd_t *p, int nWords, int nBits, int fReverse, int fVerbose) { Vec_WrdDumpBool(pFileName, p, nWords, nBits, fReverse, fVerbose); }
void Vec_WrdDumpHexOne_imctk_abc_sys(FILE *pFile, word *pSim, int nWords) { Vec_WrdDumpHexOne(pFile, pSim, nWords); }
void Vec_WrdPrintHex_imctk_abc_sys(Vec_Wrd_t *p, int nWords) { Vec_WrdPrintHex(p, nWords); }
void Vec_WrdDumpHex_imctk_abc_sys(char *pFileName, Vec_Wrd_t *p, int nWords, int fVerbose) { Vec_WrdDumpHex(pFileName, p, nWords, fVerbose); }
int Vec_WrdReadHexOne_imctk_abc_sys(char c) { return Vec_WrdReadHexOne(c); }
Vec_Wrd_t * Vec_WrdReadHex_imctk_abc_sys(char *pFileName, int *pnWords, int fVerbose) { return Vec_WrdReadHex(pFileName, pnWords, fVerbose); }
void Vec_WrdDumpBin_imctk_abc_sys(char *pFileName, Vec_Wrd_t *p, int fVerbose) { Vec_WrdDumpBin(pFileName, p, fVerbose); }
Vec_Wrd_t * Vec_WrdReadBin_imctk_abc_sys(char *pFileName, int fVerbose) { return Vec_WrdReadBin(pFileName, fVerbose); }
Vec_Bit_t * Vec_BitAlloc_imctk_abc_sys(int nCap) { return Vec_BitAlloc(nCap); }
Vec_Bit_t * Vec_BitStart_imctk_abc_sys(int nSize) { return Vec_BitStart(nSize); }
Vec_Bit_t * Vec_BitStartFull_imctk_abc_sys(int nSize) { return Vec_BitStartFull(nSize); }
Vec_Bit_t * Vec_BitDup_imctk_abc_sys(Vec_Bit_t *pVec) { return Vec_BitDup(pVec); }
void Vec_BitFree_imctk_abc_sys(Vec_Bit_t *p) { Vec_BitFree(p); }
void Vec_BitFreeP_imctk_abc_sys(Vec_Bit_t **p) { Vec_BitFreeP(p); }
int * Vec_BitReleaseArray_imctk_abc_sys(Vec_Bit_t *p) { return Vec_BitReleaseArray(p); }
int * Vec_BitArray_imctk_abc_sys(Vec_Bit_t *p) { return Vec_BitArray(p); }
int Vec_BitSize_imctk_abc_sys(Vec_Bit_t *p) { return Vec_BitSize(p); }
int Vec_BitCap_imctk_abc_sys(Vec_Bit_t *p) { return Vec_BitCap(p); }
double Vec_BitMemory_imctk_abc_sys(Vec_Bit_t *p) { return Vec_BitMemory(p); }
int Vec_BitEntry_imctk_abc_sys(Vec_Bit_t *p, int i) { return Vec_BitEntry(p, i); }
void Vec_BitWriteEntry_imctk_abc_sys(Vec_Bit_t *p, int i, int Entry) { Vec_BitWriteEntry(p, i, Entry); }
int Vec_BitAddEntry_imctk_abc_sys(Vec_Bit_t *p, int i) { return Vec_BitAddEntry(p, i); }
int Vec_BitEntryLast_imctk_abc_sys(Vec_Bit_t *p) { return Vec_BitEntryLast(p); }
void Vec_BitGrow_imctk_abc_sys(Vec_Bit_t *p, int nCapMin) { Vec_BitGrow(p, nCapMin); }
void Vec_BitFill_imctk_abc_sys(Vec_Bit_t *p, int nSize, int Fill) { Vec_BitFill(p, nSize, Fill); }
void Vec_BitFillExtra_imctk_abc_sys(Vec_Bit_t *p, int nSize, int Fill) { Vec_BitFillExtra(p, nSize, Fill); }
int Vec_BitGetEntry_imctk_abc_sys(Vec_Bit_t *p, int i) { return Vec_BitGetEntry(p, i); }
void Vec_BitSetEntry_imctk_abc_sys(Vec_Bit_t *p, int i, int Entry) { Vec_BitSetEntry(p, i, Entry); }
void Vec_BitShrink_imctk_abc_sys(Vec_Bit_t *p, int nSizeNew) { Vec_BitShrink(p, nSizeNew); }
void Vec_BitClear_imctk_abc_sys(Vec_Bit_t *p) { Vec_BitClear(p); }
void Vec_BitPush_imctk_abc_sys(Vec_Bit_t *p, int Entry) { Vec_BitPush(p, Entry); }
int Vec_BitPop_imctk_abc_sys(Vec_Bit_t *p) { return Vec_BitPop(p); }
int Vec_BitCountWord_imctk_abc_sys(unsigned int uWord) { return Vec_BitCountWord(uWord); }
int Vec_BitCount_imctk_abc_sys(Vec_Bit_t *p) { return Vec_BitCount(p); }
void Vec_BitReset_imctk_abc_sys(Vec_Bit_t *p) { Vec_BitReset(p); }
void Vec_BitPrint_imctk_abc_sys(Vec_Bit_t *p) { Vec_BitPrint(p); }
void Vec_MemAlloc__imctk_abc_sys(Vec_Mem_t *p, int nEntrySize, int LogPageSze) { Vec_MemAlloc_(p, nEntrySize, LogPageSze); }
Vec_Mem_t * Vec_MemAlloc_imctk_abc_sys(int nEntrySize, int LogPageSze) { return Vec_MemAlloc(nEntrySize, LogPageSze); }
void Vec_MemFree_imctk_abc_sys(Vec_Mem_t *p) { Vec_MemFree(p); }
void Vec_MemFreeP_imctk_abc_sys(Vec_Mem_t **p) { Vec_MemFreeP(p); }
Vec_Mem_t * Vec_MemDup_imctk_abc_sys(Vec_Mem_t *pVec) { return Vec_MemDup(pVec); }
void Vec_MemFill_imctk_abc_sys(Vec_Mem_t *pVec, int nEntries) { Vec_MemFill(pVec, nEntries); }
void Vec_MemClean_imctk_abc_sys(Vec_Mem_t *pVec, int nEntries) { Vec_MemClean(pVec, nEntries); }
int Vec_MemEntrySize_imctk_abc_sys(Vec_Mem_t *p) { return Vec_MemEntrySize(p); }
int Vec_MemEntryNum_imctk_abc_sys(Vec_Mem_t *p) { return Vec_MemEntryNum(p); }
int Vec_MemPageSize_imctk_abc_sys(Vec_Mem_t *p) { return Vec_MemPageSize(p); }
int Vec_MemPageNum_imctk_abc_sys(Vec_Mem_t *p) { return Vec_MemPageNum(p); }
double Vec_MemMemory_imctk_abc_sys(Vec_Mem_t *p) { return Vec_MemMemory(p); }
word * Vec_MemReadEntry_imctk_abc_sys(Vec_Mem_t *p, int i) { return Vec_MemReadEntry(p, i); }
word * Vec_MemReadEntryLast_imctk_abc_sys(Vec_Mem_t *p) { return Vec_MemReadEntryLast(p); }
void Vec_MemWriteEntry_imctk_abc_sys(Vec_Mem_t *p, int i, word *pEntry) { Vec_MemWriteEntry(p, i, pEntry); }
word * Vec_MemGetEntry_imctk_abc_sys(Vec_Mem_t *p, int i) { return Vec_MemGetEntry(p, i); }
void Vec_MemSetEntry_imctk_abc_sys(Vec_Mem_t *p, int i, word *pEntry) { Vec_MemSetEntry(p, i, pEntry); }
void Vec_MemPush_imctk_abc_sys(Vec_Mem_t *p, word *pEntry) { Vec_MemPush(p, pEntry); }
void Vec_MemShrink_imctk_abc_sys(Vec_Mem_t *p, int nEntriesNew) { Vec_MemShrink(p, nEntriesNew); }
void Vec_MemDumpDigit_imctk_abc_sys(FILE *pFile, int HexDigit) { Vec_MemDumpDigit(pFile, HexDigit); }
void Vec_MemDump_imctk_abc_sys(FILE *pFile, Vec_Mem_t *pVec) { Vec_MemDump(pFile, pVec); }
void Vec_MemHashAlloc_imctk_abc_sys(Vec_Mem_t *p, int nTableSize) { Vec_MemHashAlloc(p, nTableSize); }
void Vec_MemHashFree_imctk_abc_sys(Vec_Mem_t *p) { Vec_MemHashFree(p); }
unsigned int Vec_MemHashKey_imctk_abc_sys(Vec_Mem_t *p, word *pEntry) { return Vec_MemHashKey(p, pEntry); }
int * Vec_MemHashLookup_imctk_abc_sys(Vec_Mem_t *p, word *pEntry) { return Vec_MemHashLookup(p, pEntry); }
void Vec_MemHashProfile_imctk_abc_sys(Vec_Mem_t *p) { Vec_MemHashProfile(p); }
void Vec_MemHashResize_imctk_abc_sys(Vec_Mem_t *p) { Vec_MemHashResize(p); }
int Vec_MemHashInsert_imctk_abc_sys(Vec_Mem_t *p, word *pEntry) { return Vec_MemHashInsert(p, pEntry); }
Vec_Mem_t * Vec_MemAllocForTTSimple_imctk_abc_sys(int nVars) { return Vec_MemAllocForTTSimple(nVars); }
Vec_Mem_t * Vec_MemAllocForTT_imctk_abc_sys(int nVars, int fCompl) { return Vec_MemAllocForTT(nVars, fCompl); }
void Vec_MemAddMuxTT_imctk_abc_sys(Vec_Mem_t *p, int nVars) { Vec_MemAddMuxTT(p, nVars); }
void Vec_MemDumpTruthTables_imctk_abc_sys(Vec_Mem_t *p, char *pName, int nLutSize) { Vec_MemDumpTruthTables(p, pName, nLutSize); }
Vec_Wec_t * Vec_WecAlloc_imctk_abc_sys(int nCap) { return Vec_WecAlloc(nCap); }
Vec_Wec_t * Vec_WecAllocExact_imctk_abc_sys(int nCap) { return Vec_WecAllocExact(nCap); }
Vec_Wec_t * Vec_WecStart_imctk_abc_sys(int nSize) { return Vec_WecStart(nSize); }
void Vec_WecGrow_imctk_abc_sys(Vec_Wec_t *p, int nCapMin) { Vec_WecGrow(p, nCapMin); }
void Vec_WecInit_imctk_abc_sys(Vec_Wec_t *p, int nSize) { Vec_WecInit(p, nSize); }
Vec_Int_t * Vec_WecEntry_imctk_abc_sys(Vec_Wec_t *p, int i) { return Vec_WecEntry(p, i); }
Vec_Int_t * Vec_WecEntryLast_imctk_abc_sys(Vec_Wec_t *p) { return Vec_WecEntryLast(p); }
int Vec_WecEntryEntry_imctk_abc_sys(Vec_Wec_t *p, int i, int k) { return Vec_WecEntryEntry(p, i, k); }
Vec_Int_t * Vec_WecArray_imctk_abc_sys(Vec_Wec_t *p) { return Vec_WecArray(p); }
int Vec_WecLevelId_imctk_abc_sys(Vec_Wec_t *p, Vec_Int_t *vLevel) { return Vec_WecLevelId(p, vLevel); }
int Vec_WecCap_imctk_abc_sys(Vec_Wec_t *p) { return Vec_WecCap(p); }
int Vec_WecSize_imctk_abc_sys(Vec_Wec_t *p) { return Vec_WecSize(p); }
int Vec_WecLevelSize_imctk_abc_sys(Vec_Wec_t *p, int i) { return Vec_WecLevelSize(p, i); }
int Vec_WecSizeSize_imctk_abc_sys(Vec_Wec_t *p) { return Vec_WecSizeSize(p); }
int Vec_WecSizeUsed_imctk_abc_sys(Vec_Wec_t *p) { return Vec_WecSizeUsed(p); }
int Vec_WecSizeUsedLimits_imctk_abc_sys(Vec_Wec_t *p, int iStart, int iStop) { return Vec_WecSizeUsedLimits(p, iStart, iStop); }
void Vec_WecShrink_imctk_abc_sys(Vec_Wec_t *p, int nSizeNew) { Vec_WecShrink(p, nSizeNew); }
void Vec_WecClear_imctk_abc_sys(Vec_Wec_t *p) { Vec_WecClear(p); }
void Vec_WecClearLevels_imctk_abc_sys(Vec_Wec_t *p) { Vec_WecClearLevels(p); }
void Vec_WecPush_imctk_abc_sys(Vec_Wec_t *p, int Level, int Entry) { Vec_WecPush(p, Level, Entry); }
void Vec_WecPushTwo_imctk_abc_sys(Vec_Wec_t *p, int Level, int Entry1, int Entry2) { Vec_WecPushTwo(p, Level, Entry1, Entry2); }
Vec_Int_t * Vec_WecPushLevel_imctk_abc_sys(Vec_Wec_t *p) { return Vec_WecPushLevel(p); }
Vec_Int_t * Vec_WecInsertLevel_imctk_abc_sys(Vec_Wec_t *p, int i) { return Vec_WecInsertLevel(p, i); }
double Vec_WecMemory_imctk_abc_sys(Vec_Wec_t *p) { return Vec_WecMemory(p); }
void Vec_WecZero_imctk_abc_sys(Vec_Wec_t *p) { Vec_WecZero(p); }
void Vec_WecErase_imctk_abc_sys(Vec_Wec_t *p) { Vec_WecErase(p); }
void Vec_WecFree_imctk_abc_sys(Vec_Wec_t *p) { Vec_WecFree(p); }
void Vec_WecFreeP_imctk_abc_sys(Vec_Wec_t **p) { Vec_WecFreeP(p); }
void Vec_WecPushUnique_imctk_abc_sys(Vec_Wec_t *p, int Level, int Entry) { Vec_WecPushUnique(p, Level, Entry); }
Vec_Wec_t * Vec_WecDup_imctk_abc_sys(Vec_Wec_t *p) { return Vec_WecDup(p); }
int Vec_WecSortCompare1_imctk_abc_sys(Vec_Int_t *p1, Vec_Int_t *p2) { return Vec_WecSortCompare1(p1, p2); }
int Vec_WecSortCompare2_imctk_abc_sys(Vec_Int_t *p1, Vec_Int_t *p2) { return Vec_WecSortCompare2(p1, p2); }
void Vec_WecSort_imctk_abc_sys(Vec_Wec_t *p, int fReverse) { Vec_WecSort(p, fReverse); }
int Vec_WecSortCompare3_imctk_abc_sys(Vec_Int_t *p1, Vec_Int_t *p2) { return Vec_WecSortCompare3(p1, p2); }
int Vec_WecSortCompare4_imctk_abc_sys(Vec_Int_t *p1, Vec_Int_t *p2) { return Vec_WecSortCompare4(p1, p2); }
void Vec_WecSortByFirstInt_imctk_abc_sys(Vec_Wec_t *p, int fReverse) { Vec_WecSortByFirstInt(p, fReverse); }
int Vec_WecSortCompare5_imctk_abc_sys(Vec_Int_t *p1, Vec_Int_t *p2) { return Vec_WecSortCompare5(p1, p2); }
int Vec_WecSortCompare6_imctk_abc_sys(Vec_Int_t *p1, Vec_Int_t *p2) { return Vec_WecSortCompare6(p1, p2); }
void Vec_WecSortByLastInt_imctk_abc_sys(Vec_Wec_t *p, int fReverse) { Vec_WecSortByLastInt(p, fReverse); }
void Vec_WecKeepLevels_imctk_abc_sys(Vec_Wec_t *p, int Limit) { Vec_WecKeepLevels(p, Limit); }
void Vec_WecPrint_imctk_abc_sys(Vec_Wec_t *p, int fSkipSingles) { Vec_WecPrint(p, fSkipSingles); }
void Vec_WecPrintLits_imctk_abc_sys(Vec_Wec_t *p) { Vec_WecPrintLits(p); }
Vec_Wec_t * Vec_WecCreateClasses_imctk_abc_sys(Vec_Int_t *vMap) { return Vec_WecCreateClasses(vMap); }
int Vec_WecCountNonTrivial_imctk_abc_sys(Vec_Wec_t *p, int *pnUsed) { return Vec_WecCountNonTrivial(p, pnUsed); }
int Vec_WecMaxLevelSize_imctk_abc_sys(Vec_Wec_t *p) { return Vec_WecMaxLevelSize(p); }
Vec_Int_t * Vec_WecCollectFirsts_imctk_abc_sys(Vec_Wec_t *p) { return Vec_WecCollectFirsts(p); }
Vec_Ptr_t * Vec_WecConvertToVecPtr_imctk_abc_sys(Vec_Wec_t *p) { return Vec_WecConvertToVecPtr(p); }
int Vec_WecIntHasMark_imctk_abc_sys(Vec_Int_t *vVec) { return Vec_WecIntHasMark(vVec); }
void Vec_WecIntSetMark_imctk_abc_sys(Vec_Int_t *vVec) { Vec_WecIntSetMark(vVec); }
void Vec_WecIntXorMark_imctk_abc_sys(Vec_Int_t *vVec) { Vec_WecIntXorMark(vVec); }
void Vec_WecMarkLevels_imctk_abc_sys(Vec_Wec_t *vCubes, Vec_Int_t *vLevels) { Vec_WecMarkLevels(vCubes, vLevels); }
void Vec_WecUnmarkLevels_imctk_abc_sys(Vec_Wec_t *vCubes, Vec_Int_t *vLevels) { Vec_WecUnmarkLevels(vCubes, vLevels); }
void Vec_WecRemoveEmpty_imctk_abc_sys(Vec_Wec_t *vCubes) { Vec_WecRemoveEmpty(vCubes); }
void Vec_WecDumpBin_imctk_abc_sys(char *pFileName, Vec_Wec_t *p, int fVerbose) { Vec_WecDumpBin(pFileName, p, fVerbose); }
Vec_Wec_t * Vec_WecReadBin_imctk_abc_sys(char *pFileName, int fVerbose) { return Vec_WecReadBin(pFileName, fVerbose); }
unsigned int Gia_ObjCutSign_imctk_abc_sys(unsigned int ObjId) { return Gia_ObjCutSign(ObjId); }
int Gia_WordHasOneBit_imctk_abc_sys(unsigned int uWord) { return Gia_WordHasOneBit(uWord); }
int Gia_WordHasOnePair_imctk_abc_sys(unsigned int uWord) { return Gia_WordHasOnePair(uWord); }
int Gia_WordCountOnes_imctk_abc_sys(unsigned int uWord) { return Gia_WordCountOnes(uWord); }
int Gia_WordFindFirstBit_imctk_abc_sys(unsigned int uWord) { return Gia_WordFindFirstBit(uWord); }
int Gia_ManTruthIsConst0_imctk_abc_sys(unsigned int *pIn, int nVars) { return Gia_ManTruthIsConst0(pIn, nVars); }
int Gia_ManTruthIsConst1_imctk_abc_sys(unsigned int *pIn, int nVars) { return Gia_ManTruthIsConst1(pIn, nVars); }
void Gia_ManTruthCopy_imctk_abc_sys(unsigned int *pOut, unsigned int *pIn, int nVars) { Gia_ManTruthCopy(pOut, pIn, nVars); }
void Gia_ManTruthClear_imctk_abc_sys(unsigned int *pOut, int nVars) { Gia_ManTruthClear(pOut, nVars); }
void Gia_ManTruthFill_imctk_abc_sys(unsigned int *pOut, int nVars) { Gia_ManTruthFill(pOut, nVars); }
void Gia_ManTruthNot_imctk_abc_sys(unsigned int *pOut, unsigned int *pIn, int nVars) { Gia_ManTruthNot(pOut, pIn, nVars); }
int Gia_ManConst0Lit_imctk_abc_sys(void) { return Gia_ManConst0Lit(); }
int Gia_ManConst1Lit_imctk_abc_sys(void) { return Gia_ManConst1Lit(); }
int Gia_ManIsConst0Lit_imctk_abc_sys(int iLit) { return Gia_ManIsConst0Lit(iLit); }
int Gia_ManIsConst1Lit_imctk_abc_sys(int iLit) { return Gia_ManIsConst1Lit(iLit); }
int Gia_ManIsConstLit_imctk_abc_sys(int iLit) { return Gia_ManIsConstLit(iLit); }
Gia_Obj_t * Gia_Regular_imctk_abc_sys(Gia_Obj_t *p) { return Gia_Regular(p); }
Gia_Obj_t * Gia_Not_imctk_abc_sys(Gia_Obj_t *p) { return Gia_Not(p); }
Gia_Obj_t * Gia_NotCond_imctk_abc_sys(Gia_Obj_t *p, int c) { return Gia_NotCond(p, c); }
int Gia_IsComplement_imctk_abc_sys(Gia_Obj_t *p) { return Gia_IsComplement(p); }
char * Gia_ManName_imctk_abc_sys(Gia_Man_t *p) { return Gia_ManName(p); }
int Gia_ManCiNum_imctk_abc_sys(Gia_Man_t *p) { return Gia_ManCiNum(p); }
int Gia_ManCoNum_imctk_abc_sys(Gia_Man_t *p) { return Gia_ManCoNum(p); }
int Gia_ManPiNum_imctk_abc_sys(Gia_Man_t *p) { return Gia_ManPiNum(p); }
int Gia_ManPoNum_imctk_abc_sys(Gia_Man_t *p) { return Gia_ManPoNum(p); }
int Gia_ManRegNum_imctk_abc_sys(Gia_Man_t *p) { return Gia_ManRegNum(p); }
int Gia_ManObjNum_imctk_abc_sys(Gia_Man_t *p) { return Gia_ManObjNum(p); }
int Gia_ManAndNum_imctk_abc_sys(Gia_Man_t *p) { return Gia_ManAndNum(p); }
int Gia_ManXorNum_imctk_abc_sys(Gia_Man_t *p) { return Gia_ManXorNum(p); }
int Gia_ManMuxNum_imctk_abc_sys(Gia_Man_t *p) { return Gia_ManMuxNum(p); }
int Gia_ManBufNum_imctk_abc_sys(Gia_Man_t *p) { return Gia_ManBufNum(p); }
int Gia_ManAndNotBufNum_imctk_abc_sys(Gia_Man_t *p) { return Gia_ManAndNotBufNum(p); }
int Gia_ManCandNum_imctk_abc_sys(Gia_Man_t *p) { return Gia_ManCandNum(p); }
int Gia_ManConstrNum_imctk_abc_sys(Gia_Man_t *p) { return Gia_ManConstrNum(p); }
void Gia_ManFlipVerbose_imctk_abc_sys(Gia_Man_t *p) { Gia_ManFlipVerbose(p); }
int Gia_ManHasChoices_imctk_abc_sys(Gia_Man_t *p) { return Gia_ManHasChoices(p); }
int Gia_ManChoiceNum_imctk_abc_sys(Gia_Man_t *p) { return Gia_ManChoiceNum(p); }
Gia_Obj_t * Gia_ManConst0_imctk_abc_sys(Gia_Man_t *p) { return Gia_ManConst0(p); }
Gia_Obj_t * Gia_ManConst1_imctk_abc_sys(Gia_Man_t *p) { return Gia_ManConst1(p); }
Gia_Obj_t * Gia_ManObj_imctk_abc_sys(Gia_Man_t *p, int v) { return Gia_ManObj(p, v); }
Gia_Obj_t * Gia_ManCi_imctk_abc_sys(Gia_Man_t *p, int v) { return Gia_ManCi(p, v); }
Gia_Obj_t * Gia_ManCo_imctk_abc_sys(Gia_Man_t *p, int v) { return Gia_ManCo(p, v); }
Gia_Obj_t * Gia_ManPi_imctk_abc_sys(Gia_Man_t *p, int v) { return Gia_ManPi(p, v); }
Gia_Obj_t * Gia_ManPo_imctk_abc_sys(Gia_Man_t *p, int v) { return Gia_ManPo(p, v); }
Gia_Obj_t * Gia_ManRo_imctk_abc_sys(Gia_Man_t *p, int v) { return Gia_ManRo(p, v); }
Gia_Obj_t * Gia_ManRi_imctk_abc_sys(Gia_Man_t *p, int v) { return Gia_ManRi(p, v); }
int Gia_ObjId_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjId(p, pObj); }
int Gia_ObjCioId_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjCioId(pObj); }
void Gia_ObjSetCioId_imctk_abc_sys(Gia_Obj_t *pObj, int v) { Gia_ObjSetCioId(pObj, v); }
int Gia_ObjValue_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjValue(pObj); }
void Gia_ObjSetValue_imctk_abc_sys(Gia_Obj_t *pObj, int i) { Gia_ObjSetValue(pObj, i); }
int Gia_ObjPhase_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjPhase(pObj); }
int Gia_ObjPhaseReal_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjPhaseReal(pObj); }
int Gia_ObjPhaseDiff_imctk_abc_sys(Gia_Man_t *p, int i, int k) { return Gia_ObjPhaseDiff(p, i, k); }
char * Gia_ObjCiName_imctk_abc_sys(Gia_Man_t *p, int i) { return Gia_ObjCiName(p, i); }
char * Gia_ObjCoName_imctk_abc_sys(Gia_Man_t *p, int i) { return Gia_ObjCoName(p, i); }
char * Gia_ObjName_imctk_abc_sys(Gia_Man_t *p, int i) { return Gia_ObjName(p, i); }
char * Gia_ObjNameObj_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjNameObj(p, pObj); }
int Gia_ObjIsTerm_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjIsTerm(pObj); }
int Gia_ObjIsAndOrConst0_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjIsAndOrConst0(pObj); }
int Gia_ObjIsCi_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjIsCi(pObj); }
int Gia_ObjIsCo_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjIsCo(pObj); }
int Gia_ObjIsAnd_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjIsAnd(pObj); }
int Gia_ObjIsXor_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjIsXor(pObj); }
int Gia_ObjIsMuxId_imctk_abc_sys(Gia_Man_t *p, int iObj) { return Gia_ObjIsMuxId(p, iObj); }
int Gia_ObjIsMux_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjIsMux(p, pObj); }
int Gia_ObjIsAndReal_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjIsAndReal(p, pObj); }
int Gia_ObjIsBuf_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjIsBuf(pObj); }
int Gia_ObjIsAndNotBuf_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjIsAndNotBuf(pObj); }
int Gia_ObjIsCand_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjIsCand(pObj); }
int Gia_ObjIsConst0_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjIsConst0(pObj); }
int Gia_ManObjIsConst0_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ManObjIsConst0(p, pObj); }
int Gia_Obj2Lit_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_Obj2Lit(p, pObj); }
Gia_Obj_t * Gia_Lit2Obj_imctk_abc_sys(Gia_Man_t *p, int iLit) { return Gia_Lit2Obj(p, iLit); }
int Gia_ManCiLit_imctk_abc_sys(Gia_Man_t *p, int CiId) { return Gia_ManCiLit(p, CiId); }
int Gia_ManIdToCioId_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ManIdToCioId(p, Id); }
int Gia_ManCiIdToId_imctk_abc_sys(Gia_Man_t *p, int CiId) { return Gia_ManCiIdToId(p, CiId); }
int Gia_ManCoIdToId_imctk_abc_sys(Gia_Man_t *p, int CoId) { return Gia_ManCoIdToId(p, CoId); }
int Gia_ObjIsPi_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjIsPi(p, pObj); }
int Gia_ObjIsPo_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjIsPo(p, pObj); }
int Gia_ObjIsRo_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjIsRo(p, pObj); }
int Gia_ObjIsRi_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjIsRi(p, pObj); }
Gia_Obj_t * Gia_ObjRoToRi_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjRoToRi(p, pObj); }
Gia_Obj_t * Gia_ObjRiToRo_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjRiToRo(p, pObj); }
int Gia_ObjRoToRiId_imctk_abc_sys(Gia_Man_t *p, int ObjId) { return Gia_ObjRoToRiId(p, ObjId); }
int Gia_ObjRiToRoId_imctk_abc_sys(Gia_Man_t *p, int ObjId) { return Gia_ObjRiToRoId(p, ObjId); }
int Gia_ObjDiff0_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjDiff0(pObj); }
int Gia_ObjDiff1_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjDiff1(pObj); }
int Gia_ObjFaninC0_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjFaninC0(pObj); }
int Gia_ObjFaninC1_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjFaninC1(pObj); }
int Gia_ObjFaninC2_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjFaninC2(p, pObj); }
int Gia_ObjFaninC_imctk_abc_sys(Gia_Obj_t *pObj, int n) { return Gia_ObjFaninC(pObj, n); }
Gia_Obj_t * Gia_ObjFanin0_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjFanin0(pObj); }
Gia_Obj_t * Gia_ObjFanin1_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjFanin1(pObj); }
Gia_Obj_t * Gia_ObjFanin2_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjFanin2(p, pObj); }
Gia_Obj_t * Gia_ObjFanin_imctk_abc_sys(Gia_Obj_t *pObj, int n) { return Gia_ObjFanin(pObj, n); }
Gia_Obj_t * Gia_ObjChild0_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjChild0(pObj); }
Gia_Obj_t * Gia_ObjChild1_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjChild1(pObj); }
Gia_Obj_t * Gia_ObjChild2_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjChild2(p, pObj); }
int Gia_ObjFaninId0_imctk_abc_sys(Gia_Obj_t *pObj, int ObjId) { return Gia_ObjFaninId0(pObj, ObjId); }
int Gia_ObjFaninId1_imctk_abc_sys(Gia_Obj_t *pObj, int ObjId) { return Gia_ObjFaninId1(pObj, ObjId); }
int Gia_ObjFaninId2_imctk_abc_sys(Gia_Man_t *p, int ObjId) { return Gia_ObjFaninId2(p, ObjId); }
int Gia_ObjFaninId_imctk_abc_sys(Gia_Obj_t *pObj, int ObjId, int n) { return Gia_ObjFaninId(pObj, ObjId, n); }
int Gia_ObjFaninId0p_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjFaninId0p(p, pObj); }
int Gia_ObjFaninId1p_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjFaninId1p(p, pObj); }
int Gia_ObjFaninId2p_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjFaninId2p(p, pObj); }
int Gia_ObjFaninIdp_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj, int n) { return Gia_ObjFaninIdp(p, pObj, n); }
int Gia_ObjFaninLit0_imctk_abc_sys(Gia_Obj_t *pObj, int ObjId) { return Gia_ObjFaninLit0(pObj, ObjId); }
int Gia_ObjFaninLit1_imctk_abc_sys(Gia_Obj_t *pObj, int ObjId) { return Gia_ObjFaninLit1(pObj, ObjId); }
int Gia_ObjFaninLit2_imctk_abc_sys(Gia_Man_t *p, int ObjId) { return Gia_ObjFaninLit2(p, ObjId); }
int Gia_ObjFaninLit0p_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjFaninLit0p(p, pObj); }
int Gia_ObjFaninLit1p_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjFaninLit1p(p, pObj); }
int Gia_ObjFaninLit2p_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjFaninLit2p(p, pObj); }
void Gia_ObjFlipFaninC0_imctk_abc_sys(Gia_Obj_t *pObj) { Gia_ObjFlipFaninC0(pObj); }
int Gia_ObjFaninNum_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjFaninNum(p, pObj); }
int Gia_ObjWhatFanin_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj, Gia_Obj_t *pFanin) { return Gia_ObjWhatFanin(p, pObj, pFanin); }
int Gia_ManCoDriverId_imctk_abc_sys(Gia_Man_t *p, int iCoIndex) { return Gia_ManCoDriverId(p, iCoIndex); }
int Gia_ManPoIsConst_imctk_abc_sys(Gia_Man_t *p, int iPoIndex) { return Gia_ManPoIsConst(p, iPoIndex); }
int Gia_ManPoIsConst0_imctk_abc_sys(Gia_Man_t *p, int iPoIndex) { return Gia_ManPoIsConst0(p, iPoIndex); }
int Gia_ManPoIsConst1_imctk_abc_sys(Gia_Man_t *p, int iPoIndex) { return Gia_ManPoIsConst1(p, iPoIndex); }
Gia_Obj_t * Gia_ObjCopy_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjCopy(p, pObj); }
int Gia_ObjLitCopy_imctk_abc_sys(Gia_Man_t *p, int iLit) { return Gia_ObjLitCopy(p, iLit); }
int Gia_ObjFanin0Copy_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjFanin0Copy(pObj); }
int Gia_ObjFanin1Copy_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjFanin1Copy(pObj); }
int Gia_ObjFanin2Copy_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjFanin2Copy(p, pObj); }
int Gia_ObjCopyF_imctk_abc_sys(Gia_Man_t *p, int f, Gia_Obj_t *pObj) { return Gia_ObjCopyF(p, f, pObj); }
void Gia_ObjSetCopyF_imctk_abc_sys(Gia_Man_t *p, int f, Gia_Obj_t *pObj, int iLit) { Gia_ObjSetCopyF(p, f, pObj, iLit); }
int Gia_ObjCopyArray_imctk_abc_sys(Gia_Man_t *p, int iObj) { return Gia_ObjCopyArray(p, iObj); }
void Gia_ObjSetCopyArray_imctk_abc_sys(Gia_Man_t *p, int iObj, int iLit) { Gia_ObjSetCopyArray(p, iObj, iLit); }
void Gia_ManCleanCopyArray_imctk_abc_sys(Gia_Man_t *p) { Gia_ManCleanCopyArray(p); }
int Gia_ObjCopy2Array_imctk_abc_sys(Gia_Man_t *p, int iObj) { return Gia_ObjCopy2Array(p, iObj); }
void Gia_ObjSetCopy2Array_imctk_abc_sys(Gia_Man_t *p, int iObj, int iLit) { Gia_ObjSetCopy2Array(p, iObj, iLit); }
void Gia_ManCleanCopy2Array_imctk_abc_sys(Gia_Man_t *p) { Gia_ManCleanCopy2Array(p); }
int Gia_ObjFanin0CopyF_imctk_abc_sys(Gia_Man_t *p, int f, Gia_Obj_t *pObj) { return Gia_ObjFanin0CopyF(p, f, pObj); }
int Gia_ObjFanin1CopyF_imctk_abc_sys(Gia_Man_t *p, int f, Gia_Obj_t *pObj) { return Gia_ObjFanin1CopyF(p, f, pObj); }
int Gia_ObjFanin0CopyArray_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjFanin0CopyArray(p, pObj); }
int Gia_ObjFanin1CopyArray_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjFanin1CopyArray(p, pObj); }
Gia_Obj_t * Gia_ObjFromLit_imctk_abc_sys(Gia_Man_t *p, int iLit) { return Gia_ObjFromLit(p, iLit); }
int Gia_ObjToLit_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjToLit(p, pObj); }
int Gia_ObjPhaseRealLit_imctk_abc_sys(Gia_Man_t *p, int iLit) { return Gia_ObjPhaseRealLit(p, iLit); }
int Gia_ObjLevelId_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjLevelId(p, Id); }
int Gia_ObjLevel_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjLevel(p, pObj); }
void Gia_ObjUpdateLevelId_imctk_abc_sys(Gia_Man_t *p, int Id, int l) { Gia_ObjUpdateLevelId(p, Id, l); }
void Gia_ObjSetLevelId_imctk_abc_sys(Gia_Man_t *p, int Id, int l) { Gia_ObjSetLevelId(p, Id, l); }
void Gia_ObjSetLevel_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj, int l) { Gia_ObjSetLevel(p, pObj, l); }
void Gia_ObjSetCoLevel_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { Gia_ObjSetCoLevel(p, pObj); }
void Gia_ObjSetBufLevel_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { Gia_ObjSetBufLevel(p, pObj); }
void Gia_ObjSetAndLevel_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { Gia_ObjSetAndLevel(p, pObj); }
void Gia_ObjSetXorLevel_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { Gia_ObjSetXorLevel(p, pObj); }
void Gia_ObjSetMuxLevel_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { Gia_ObjSetMuxLevel(p, pObj); }
void Gia_ObjSetGateLevel_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { Gia_ObjSetGateLevel(p, pObj); }
int Gia_ObjHasNumId_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjHasNumId(p, Id); }
int Gia_ObjNumId_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjNumId(p, Id); }
int Gia_ObjNum_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjNum(p, pObj); }
void Gia_ObjSetNumId_imctk_abc_sys(Gia_Man_t *p, int Id, int n) { Gia_ObjSetNumId(p, Id, n); }
void Gia_ObjSetNum_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj, int n) { Gia_ObjSetNum(p, pObj, n); }
void Gia_ObjResetNumId_imctk_abc_sys(Gia_Man_t *p, int Id) { Gia_ObjResetNumId(p, Id); }
int Gia_ObjRefNumId_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjRefNumId(p, Id); }
int Gia_ObjRefIncId_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjRefIncId(p, Id); }
int Gia_ObjRefDecId_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjRefDecId(p, Id); }
int Gia_ObjRefNum_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjRefNum(p, pObj); }
int Gia_ObjRefInc_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjRefInc(p, pObj); }
int Gia_ObjRefDec_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjRefDec(p, pObj); }
void Gia_ObjRefFanin0Inc_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { Gia_ObjRefFanin0Inc(p, pObj); }
void Gia_ObjRefFanin1Inc_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { Gia_ObjRefFanin1Inc(p, pObj); }
void Gia_ObjRefFanin2Inc_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { Gia_ObjRefFanin2Inc(p, pObj); }
void Gia_ObjRefFanin0Dec_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { Gia_ObjRefFanin0Dec(p, pObj); }
void Gia_ObjRefFanin1Dec_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { Gia_ObjRefFanin1Dec(p, pObj); }
void Gia_ObjRefFanin2Dec_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { Gia_ObjRefFanin2Dec(p, pObj); }
int Gia_ObjLutRefNumId_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjLutRefNumId(p, Id); }
int Gia_ObjLutRefIncId_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjLutRefIncId(p, Id); }
int Gia_ObjLutRefDecId_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjLutRefDecId(p, Id); }
int Gia_ObjLutRefNum_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjLutRefNum(p, pObj); }
int Gia_ObjLutRefInc_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjLutRefInc(p, pObj); }
int Gia_ObjLutRefDec_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjLutRefDec(p, pObj); }
void Gia_ObjSetTravIdCurrent_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { Gia_ObjSetTravIdCurrent(p, pObj); }
void Gia_ObjSetTravIdPrevious_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { Gia_ObjSetTravIdPrevious(p, pObj); }
int Gia_ObjIsTravIdCurrent_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjIsTravIdCurrent(p, pObj); }
int Gia_ObjIsTravIdPrevious_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjIsTravIdPrevious(p, pObj); }
int Gia_ObjUpdateTravIdCurrent_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjUpdateTravIdCurrent(p, pObj); }
int Gia_ObjUpdateTravIdPrevious_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjUpdateTravIdPrevious(p, pObj); }
void Gia_ObjSetTravIdCurrentId_imctk_abc_sys(Gia_Man_t *p, int Id) { Gia_ObjSetTravIdCurrentId(p, Id); }
void Gia_ObjSetTravIdPreviousId_imctk_abc_sys(Gia_Man_t *p, int Id) { Gia_ObjSetTravIdPreviousId(p, Id); }
int Gia_ObjIsTravIdCurrentId_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjIsTravIdCurrentId(p, Id); }
int Gia_ObjIsTravIdPreviousId_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjIsTravIdPreviousId(p, Id); }
int Gia_ObjUpdateTravIdCurrentId_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjUpdateTravIdCurrentId(p, Id); }
int Gia_ObjUpdateTravIdPreviousId_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjUpdateTravIdPreviousId(p, Id); }
void Gia_ManTimeClean_imctk_abc_sys(Gia_Man_t *p) { Gia_ManTimeClean(p); }
void Gia_ManTimeStart_imctk_abc_sys(Gia_Man_t *p) { Gia_ManTimeStart(p); }
void Gia_ManTimeStop_imctk_abc_sys(Gia_Man_t *p) { Gia_ManTimeStop(p); }
float Gia_ObjTimeArrival_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjTimeArrival(p, Id); }
float Gia_ObjTimeRequired_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjTimeRequired(p, Id); }
float Gia_ObjTimeSlack_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjTimeSlack(p, Id); }
float Gia_ObjTimeArrivalObj_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjTimeArrivalObj(p, pObj); }
float Gia_ObjTimeRequiredObj_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjTimeRequiredObj(p, pObj); }
float Gia_ObjTimeSlackObj_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjTimeSlackObj(p, pObj); }
void Gia_ObjSetTimeArrival_imctk_abc_sys(Gia_Man_t *p, int Id, float t) { Gia_ObjSetTimeArrival(p, Id, t); }
void Gia_ObjSetTimeRequired_imctk_abc_sys(Gia_Man_t *p, int Id, float t) { Gia_ObjSetTimeRequired(p, Id, t); }
void Gia_ObjSetTimeSlack_imctk_abc_sys(Gia_Man_t *p, int Id, float t) { Gia_ObjSetTimeSlack(p, Id, t); }
void Gia_ObjSetTimeArrivalObj_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj, float t) { Gia_ObjSetTimeArrivalObj(p, pObj, t); }
void Gia_ObjSetTimeRequiredObj_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj, float t) { Gia_ObjSetTimeRequiredObj(p, pObj, t); }
void Gia_ObjSetTimeSlackObj_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj, float t) { Gia_ObjSetTimeSlackObj(p, pObj, t); }
int Gia_ObjSimWords_imctk_abc_sys(Gia_Man_t *p) { return Gia_ObjSimWords(p); }
word * Gia_ObjSimPi_imctk_abc_sys(Gia_Man_t *p, int PiId) { return Gia_ObjSimPi(p, PiId); }
word * Gia_ObjSim_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjSim(p, Id); }
word * Gia_ObjSimObj_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjSimObj(p, pObj); }
Gia_Obj_t * Gia_ManAppendObj_imctk_abc_sys(Gia_Man_t *p) { return Gia_ManAppendObj(p); }
int Gia_ManAppendCi_imctk_abc_sys(Gia_Man_t *p) { return Gia_ManAppendCi(p); }
int Gia_ManAppendAnd_imctk_abc_sys(Gia_Man_t *p, int iLit0, int iLit1) { return Gia_ManAppendAnd(p, iLit0, iLit1); }
int Gia_ManAppendXorReal_imctk_abc_sys(Gia_Man_t *p, int iLit0, int iLit1) { return Gia_ManAppendXorReal(p, iLit0, iLit1); }
int Gia_ManAppendMuxReal_imctk_abc_sys(Gia_Man_t *p, int iLitC, int iLit1, int iLit0) { return Gia_ManAppendMuxReal(p, iLitC, iLit1, iLit0); }
int Gia_ManAppendBuf_imctk_abc_sys(Gia_Man_t *p, int iLit) { return Gia_ManAppendBuf(p, iLit); }
int Gia_ManAppendCo_imctk_abc_sys(Gia_Man_t *p, int iLit0) { return Gia_ManAppendCo(p, iLit0); }
int Gia_ManAppendOr_imctk_abc_sys(Gia_Man_t *p, int iLit0, int iLit1) { return Gia_ManAppendOr(p, iLit0, iLit1); }
int Gia_ManAppendMux_imctk_abc_sys(Gia_Man_t *p, int iCtrl, int iData1, int iData0) { return Gia_ManAppendMux(p, iCtrl, iData1, iData0); }
int Gia_ManAppendMaj_imctk_abc_sys(Gia_Man_t *p, int iData0, int iData1, int iData2) { return Gia_ManAppendMaj(p, iData0, iData1, iData2); }
int Gia_ManAppendXor_imctk_abc_sys(Gia_Man_t *p, int iLit0, int iLit1) { return Gia_ManAppendXor(p, iLit0, iLit1); }
int Gia_ManAppendAnd2_imctk_abc_sys(Gia_Man_t *p, int iLit0, int iLit1) { return Gia_ManAppendAnd2(p, iLit0, iLit1); }
int Gia_ManAppendOr2_imctk_abc_sys(Gia_Man_t *p, int iLit0, int iLit1) { return Gia_ManAppendOr2(p, iLit0, iLit1); }
int Gia_ManAppendMux2_imctk_abc_sys(Gia_Man_t *p, int iCtrl, int iData1, int iData0) { return Gia_ManAppendMux2(p, iCtrl, iData1, iData0); }
int Gia_ManAppendMaj2_imctk_abc_sys(Gia_Man_t *p, int iData0, int iData1, int iData2) { return Gia_ManAppendMaj2(p, iData0, iData1, iData2); }
int Gia_ManAppendXor2_imctk_abc_sys(Gia_Man_t *p, int iLit0, int iLit1) { return Gia_ManAppendXor2(p, iLit0, iLit1); }
int Gia_ManAppendXorReal2_imctk_abc_sys(Gia_Man_t *p, int iLit0, int iLit1) { return Gia_ManAppendXorReal2(p, iLit0, iLit1); }
void Gia_ManPatchCoDriver_imctk_abc_sys(Gia_Man_t *p, int iCoIndex, int iLit0) { Gia_ManPatchCoDriver(p, iCoIndex, iLit0); }
int Gia_XsimNotCond_imctk_abc_sys(int Value, int fCompl) { return Gia_XsimNotCond(Value, fCompl); }
int Gia_XsimAndCond_imctk_abc_sys(int Value0, int fCompl0, int Value1, int fCompl1) { return Gia_XsimAndCond(Value0, fCompl0, Value1, fCompl1); }
void Gia_ObjTerSimSetC_imctk_abc_sys(Gia_Obj_t *pObj) { Gia_ObjTerSimSetC(pObj); }
void Gia_ObjTerSimSet0_imctk_abc_sys(Gia_Obj_t *pObj) { Gia_ObjTerSimSet0(pObj); }
void Gia_ObjTerSimSet1_imctk_abc_sys(Gia_Obj_t *pObj) { Gia_ObjTerSimSet1(pObj); }
void Gia_ObjTerSimSetX_imctk_abc_sys(Gia_Obj_t *pObj) { Gia_ObjTerSimSetX(pObj); }
int Gia_ObjTerSimGetC_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjTerSimGetC(pObj); }
int Gia_ObjTerSimGet0_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjTerSimGet0(pObj); }
int Gia_ObjTerSimGet1_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjTerSimGet1(pObj); }
int Gia_ObjTerSimGetX_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjTerSimGetX(pObj); }
int Gia_ObjTerSimGet0Fanin0_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjTerSimGet0Fanin0(pObj); }
int Gia_ObjTerSimGet1Fanin0_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjTerSimGet1Fanin0(pObj); }
int Gia_ObjTerSimGet0Fanin1_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjTerSimGet0Fanin1(pObj); }
int Gia_ObjTerSimGet1Fanin1_imctk_abc_sys(Gia_Obj_t *pObj) { return Gia_ObjTerSimGet1Fanin1(pObj); }
void Gia_ObjTerSimAnd_imctk_abc_sys(Gia_Obj_t *pObj) { Gia_ObjTerSimAnd(pObj); }
void Gia_ObjTerSimCo_imctk_abc_sys(Gia_Obj_t *pObj) { Gia_ObjTerSimCo(pObj); }
void Gia_ObjTerSimRo_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { Gia_ObjTerSimRo(p, pObj); }
void Gia_ObjTerSimPrint_imctk_abc_sys(Gia_Obj_t *pObj) { Gia_ObjTerSimPrint(pObj); }
int Gia_AigerReadInt_imctk_abc_sys(unsigned char *pPos) { return Gia_AigerReadInt(pPos); }
void Gia_AigerWriteInt_imctk_abc_sys(unsigned char *pPos, int Value) { Gia_AigerWriteInt(pPos, Value); }
unsigned int Gia_AigerReadUnsigned_imctk_abc_sys(unsigned char **ppPos) { return Gia_AigerReadUnsigned(ppPos); }
void Gia_AigerWriteUnsigned_imctk_abc_sys(Vec_Str_t *vStr, unsigned int x) { Gia_AigerWriteUnsigned(vStr, x); }
void Gia_AigerWriteUnsignedFile_imctk_abc_sys(FILE *pFile, unsigned int x) { Gia_AigerWriteUnsignedFile(pFile, x); }
int Gia_AigerWriteUnsignedBuffer_imctk_abc_sys(unsigned char *pBuffer, int Pos, unsigned int x) { return Gia_AigerWriteUnsignedBuffer(pBuffer, Pos, x); }
Gia_Obj_t * Gia_ObjReprObj_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjReprObj(p, Id); }
int Gia_ObjRepr_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjRepr(p, Id); }
void Gia_ObjSetRepr_imctk_abc_sys(Gia_Man_t *p, int Id, int Num) { Gia_ObjSetRepr(p, Id, Num); }
void Gia_ObjSetReprRev_imctk_abc_sys(Gia_Man_t *p, int Id, int Num) { Gia_ObjSetReprRev(p, Id, Num); }
void Gia_ObjUnsetRepr_imctk_abc_sys(Gia_Man_t *p, int Id) { Gia_ObjUnsetRepr(p, Id); }
int Gia_ObjHasRepr_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjHasRepr(p, Id); }
int Gia_ObjReprSelf_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjReprSelf(p, Id); }
int Gia_ObjSibl_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjSibl(p, Id); }
Gia_Obj_t * Gia_ObjSiblObj_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjSiblObj(p, Id); }
int Gia_ObjProved_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjProved(p, Id); }
void Gia_ObjSetProved_imctk_abc_sys(Gia_Man_t *p, int Id) { Gia_ObjSetProved(p, Id); }
void Gia_ObjUnsetProved_imctk_abc_sys(Gia_Man_t *p, int Id) { Gia_ObjUnsetProved(p, Id); }
int Gia_ObjFailed_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjFailed(p, Id); }
void Gia_ObjSetFailed_imctk_abc_sys(Gia_Man_t *p, int Id) { Gia_ObjSetFailed(p, Id); }
int Gia_ObjColor_imctk_abc_sys(Gia_Man_t *p, int Id, int c) { return Gia_ObjColor(p, Id, c); }
int Gia_ObjColors_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjColors(p, Id); }
void Gia_ObjSetColor_imctk_abc_sys(Gia_Man_t *p, int Id, int c) { Gia_ObjSetColor(p, Id, c); }
void Gia_ObjSetColors_imctk_abc_sys(Gia_Man_t *p, int Id) { Gia_ObjSetColors(p, Id); }
int Gia_ObjVisitColor_imctk_abc_sys(Gia_Man_t *p, int Id, int c) { return Gia_ObjVisitColor(p, Id, c); }
int Gia_ObjDiffColors_imctk_abc_sys(Gia_Man_t *p, int i, int j) { return Gia_ObjDiffColors(p, i, j); }
int Gia_ObjDiffColors2_imctk_abc_sys(Gia_Man_t *p, int i, int j) { return Gia_ObjDiffColors2(p, i, j); }
Gia_Obj_t * Gia_ObjNextObj_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjNextObj(p, Id); }
int Gia_ObjNext_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjNext(p, Id); }
void Gia_ObjSetNext_imctk_abc_sys(Gia_Man_t *p, int Id, int Num) { Gia_ObjSetNext(p, Id, Num); }
int Gia_ObjIsConst_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjIsConst(p, Id); }
int Gia_ObjIsHead_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjIsHead(p, Id); }
int Gia_ObjIsNone_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjIsNone(p, Id); }
int Gia_ObjIsTail_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjIsTail(p, Id); }
int Gia_ObjIsClass_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjIsClass(p, Id); }
int Gia_ObjHasSameRepr_imctk_abc_sys(Gia_Man_t *p, int i, int k) { return Gia_ObjHasSameRepr(p, i, k); }
int Gia_ObjIsFailedPair_imctk_abc_sys(Gia_Man_t *p, int i, int k) { return Gia_ObjIsFailedPair(p, i, k); }
int Gia_ClassIsPair_imctk_abc_sys(Gia_Man_t *p, int i) { return Gia_ClassIsPair(p, i); }
void Gia_ClassUndoPair_imctk_abc_sys(Gia_Man_t *p, int i) { Gia_ClassUndoPair(p, i); }
int Gia_ObjFoffsetId_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjFoffsetId(p, Id); }
int Gia_ObjFoffset_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjFoffset(p, pObj); }
int Gia_ObjFanoutNumId_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjFanoutNumId(p, Id); }
int Gia_ObjFanoutNum_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjFanoutNum(p, pObj); }
int Gia_ObjFanoutId_imctk_abc_sys(Gia_Man_t *p, int Id, int i) { return Gia_ObjFanoutId(p, Id, i); }
Gia_Obj_t * Gia_ObjFanout0_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj) { return Gia_ObjFanout0(p, pObj); }
Gia_Obj_t * Gia_ObjFanout_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj, int i) { return Gia_ObjFanout(p, pObj, i); }
void Gia_ObjSetFanout_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj, int i, Gia_Obj_t *pFan) { Gia_ObjSetFanout(p, pObj, i, pFan); }
void Gia_ObjSetFanoutInt_imctk_abc_sys(Gia_Man_t *p, Gia_Obj_t *pObj, int i, int x) { Gia_ObjSetFanoutInt(p, pObj, i, x); }
int Gia_ManHasMapping_imctk_abc_sys(Gia_Man_t *p) { return Gia_ManHasMapping(p); }
int Gia_ObjIsLut_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjIsLut(p, Id); }
int Gia_ObjLutSize_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjLutSize(p, Id); }
int * Gia_ObjLutFanins_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjLutFanins(p, Id); }
int Gia_ObjLutFanin_imctk_abc_sys(Gia_Man_t *p, int Id, int i) { return Gia_ObjLutFanin(p, Id, i); }
int Gia_ObjLutMuxId_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjLutMuxId(p, Id); }
int Gia_ObjLutIsMux_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjLutIsMux(p, Id); }
int Gia_ManHasMapping2_imctk_abc_sys(Gia_Man_t *p) { return Gia_ManHasMapping2(p); }
int Gia_ObjIsLut2_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjIsLut2(p, Id); }
int Gia_ObjLutSize2_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjLutSize2(p, Id); }
Vec_Int_t * Gia_ObjLutFanins2_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjLutFanins2(p, Id); }
int Gia_ObjLutFanin2_imctk_abc_sys(Gia_Man_t *p, int Id, int i) { return Gia_ObjLutFanin2(p, Id, i); }
int Gia_ObjLutFanoutNum2_imctk_abc_sys(Gia_Man_t *p, int Id) { return Gia_ObjLutFanoutNum2(p, Id); }
int Gia_ObjLutFanout2_imctk_abc_sys(Gia_Man_t *p, int Id, int i) { return Gia_ObjLutFanout2(p, Id, i); }
int Gia_ManHasCellMapping_imctk_abc_sys(Gia_Man_t *p) { return Gia_ManHasCellMapping(p); }
int Gia_ObjIsCell_imctk_abc_sys(Gia_Man_t *p, int iLit) { return Gia_ObjIsCell(p, iLit); }
int Gia_ObjIsCellInv_imctk_abc_sys(Gia_Man_t *p, int iLit) { return Gia_ObjIsCellInv(p, iLit); }
int Gia_ObjIsCellBuf_imctk_abc_sys(Gia_Man_t *p, int iLit) { return Gia_ObjIsCellBuf(p, iLit); }
int Gia_ObjCellSize_imctk_abc_sys(Gia_Man_t *p, int iLit) { return Gia_ObjCellSize(p, iLit); }
int * Gia_ObjCellFanins_imctk_abc_sys(Gia_Man_t *p, int iLit) { return Gia_ObjCellFanins(p, iLit); }
int Gia_ObjCellFanin_imctk_abc_sys(Gia_Man_t *p, int iLit, int i) { return Gia_ObjCellFanin(p, iLit, i); }
int Gia_ObjCellId_imctk_abc_sys(Gia_Man_t *p, int iLit) { return Gia_ObjCellId(p, iLit); }
Aig_Cut_t * Aig_ObjCuts_imctk_abc_sys(Aig_ManCut_t *p, Aig_Obj_t *pObj) { return Aig_ObjCuts(p, pObj); }
void Aig_ObjSetCuts_imctk_abc_sys(Aig_ManCut_t *p, Aig_Obj_t *pObj, Aig_Cut_t *pCuts) { Aig_ObjSetCuts(p, pObj, pCuts); }
int Aig_CutLeaveNum_imctk_abc_sys(Aig_Cut_t *pCut) { return Aig_CutLeaveNum(pCut); }
int * Aig_CutLeaves_imctk_abc_sys(Aig_Cut_t *pCut) { return Aig_CutLeaves(pCut); }
unsigned int * Aig_CutTruth_imctk_abc_sys(Aig_Cut_t *pCut) { return Aig_CutTruth(pCut); }
Aig_Cut_t * Aig_CutNext_imctk_abc_sys(Aig_Cut_t *pCut) { return Aig_CutNext(pCut); }
unsigned int Aig_ObjCutSign_imctk_abc_sys(unsigned int ObjId) { return Aig_ObjCutSign(ObjId); }
int Aig_WordCountOnes_imctk_abc_sys(unsigned int uWord) { return Aig_WordCountOnes(uWord); }
int Aig_WordFindFirstBit_imctk_abc_sys(unsigned int uWord) { return Aig_WordFindFirstBit(uWord); }
Aig_Obj_t * Aig_Regular_imctk_abc_sys(Aig_Obj_t *p) { return Aig_Regular(p); }
Aig_Obj_t * Aig_Not_imctk_abc_sys(Aig_Obj_t *p) { return Aig_Not(p); }
Aig_Obj_t * Aig_NotCond_imctk_abc_sys(Aig_Obj_t *p, int c) { return Aig_NotCond(p, c); }
int Aig_IsComplement_imctk_abc_sys(Aig_Obj_t *p) { return Aig_IsComplement(p); }
int Aig_ManCiNum_imctk_abc_sys(Aig_Man_t *p) { return Aig_ManCiNum(p); }
int Aig_ManCoNum_imctk_abc_sys(Aig_Man_t *p) { return Aig_ManCoNum(p); }
int Aig_ManBufNum_imctk_abc_sys(Aig_Man_t *p) { return Aig_ManBufNum(p); }
int Aig_ManAndNum_imctk_abc_sys(Aig_Man_t *p) { return Aig_ManAndNum(p); }
int Aig_ManExorNum_imctk_abc_sys(Aig_Man_t *p) { return Aig_ManExorNum(p); }
int Aig_ManNodeNum_imctk_abc_sys(Aig_Man_t *p) { return Aig_ManNodeNum(p); }
int Aig_ManGetCost_imctk_abc_sys(Aig_Man_t *p) { return Aig_ManGetCost(p); }
int Aig_ManObjNum_imctk_abc_sys(Aig_Man_t *p) { return Aig_ManObjNum(p); }
int Aig_ManObjNumMax_imctk_abc_sys(Aig_Man_t *p) { return Aig_ManObjNumMax(p); }
int Aig_ManRegNum_imctk_abc_sys(Aig_Man_t *p) { return Aig_ManRegNum(p); }
int Aig_ManConstrNum_imctk_abc_sys(Aig_Man_t *p) { return Aig_ManConstrNum(p); }
Aig_Obj_t * Aig_ManConst0_imctk_abc_sys(Aig_Man_t *p) { return Aig_ManConst0(p); }
Aig_Obj_t * Aig_ManConst1_imctk_abc_sys(Aig_Man_t *p) { return Aig_ManConst1(p); }
Aig_Obj_t * Aig_ManGhost_imctk_abc_sys(Aig_Man_t *p) { return Aig_ManGhost(p); }
Aig_Obj_t * Aig_ManCi_imctk_abc_sys(Aig_Man_t *p, int i) { return Aig_ManCi(p, i); }
Aig_Obj_t * Aig_ManCo_imctk_abc_sys(Aig_Man_t *p, int i) { return Aig_ManCo(p, i); }
Aig_Obj_t * Aig_ManLo_imctk_abc_sys(Aig_Man_t *p, int i) { return Aig_ManLo(p, i); }
Aig_Obj_t * Aig_ManLi_imctk_abc_sys(Aig_Man_t *p, int i) { return Aig_ManLi(p, i); }
Aig_Obj_t * Aig_ManObj_imctk_abc_sys(Aig_Man_t *p, int i) { return Aig_ManObj(p, i); }
Aig_Type_t Aig_ObjType_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjType(pObj); }
int Aig_ObjIsNone_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjIsNone(pObj); }
int Aig_ObjIsConst1_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjIsConst1(pObj); }
int Aig_ObjIsCi_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjIsCi(pObj); }
int Aig_ObjIsCo_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjIsCo(pObj); }
int Aig_ObjIsBuf_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjIsBuf(pObj); }
int Aig_ObjIsAnd_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjIsAnd(pObj); }
int Aig_ObjIsExor_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjIsExor(pObj); }
int Aig_ObjIsNode_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjIsNode(pObj); }
int Aig_ObjIsTerm_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjIsTerm(pObj); }
int Aig_ObjIsHash_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjIsHash(pObj); }
int Aig_ObjIsChoice_imctk_abc_sys(Aig_Man_t *p, Aig_Obj_t *pObj) { return Aig_ObjIsChoice(p, pObj); }
int Aig_ObjIsCand_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjIsCand(pObj); }
int Aig_ObjCioId_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjCioId(pObj); }
int Aig_ObjId_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjId(pObj); }
int Aig_ObjIsMarkA_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjIsMarkA(pObj); }
void Aig_ObjSetMarkA_imctk_abc_sys(Aig_Obj_t *pObj) { Aig_ObjSetMarkA(pObj); }
void Aig_ObjClearMarkA_imctk_abc_sys(Aig_Obj_t *pObj) { Aig_ObjClearMarkA(pObj); }
void Aig_ObjSetTravId_imctk_abc_sys(Aig_Obj_t *pObj, int TravId) { Aig_ObjSetTravId(pObj, TravId); }
void Aig_ObjSetTravIdCurrent_imctk_abc_sys(Aig_Man_t *p, Aig_Obj_t *pObj) { Aig_ObjSetTravIdCurrent(p, pObj); }
void Aig_ObjSetTravIdPrevious_imctk_abc_sys(Aig_Man_t *p, Aig_Obj_t *pObj) { Aig_ObjSetTravIdPrevious(p, pObj); }
int Aig_ObjIsTravIdCurrent_imctk_abc_sys(Aig_Man_t *p, Aig_Obj_t *pObj) { return Aig_ObjIsTravIdCurrent(p, pObj); }
int Aig_ObjIsTravIdPrevious_imctk_abc_sys(Aig_Man_t *p, Aig_Obj_t *pObj) { return Aig_ObjIsTravIdPrevious(p, pObj); }
int Aig_ObjPhase_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjPhase(pObj); }
int Aig_ObjPhaseReal_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjPhaseReal(pObj); }
int Aig_ObjRefs_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjRefs(pObj); }
void Aig_ObjRef_imctk_abc_sys(Aig_Obj_t *pObj) { Aig_ObjRef(pObj); }
void Aig_ObjDeref_imctk_abc_sys(Aig_Obj_t *pObj) { Aig_ObjDeref(pObj); }
void Aig_ObjClearRef_imctk_abc_sys(Aig_Obj_t *pObj) { Aig_ObjClearRef(pObj); }
int Aig_ObjFaninId0_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjFaninId0(pObj); }
int Aig_ObjFaninId1_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjFaninId1(pObj); }
int Aig_ObjFaninC0_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjFaninC0(pObj); }
int Aig_ObjFaninC1_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjFaninC1(pObj); }
Aig_Obj_t * Aig_ObjFanin0_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjFanin0(pObj); }
Aig_Obj_t * Aig_ObjFanin1_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjFanin1(pObj); }
Aig_Obj_t * Aig_ObjChild0_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjChild0(pObj); }
Aig_Obj_t * Aig_ObjChild1_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjChild1(pObj); }
Aig_Obj_t * Aig_ObjChild0Copy_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjChild0Copy(pObj); }
Aig_Obj_t * Aig_ObjChild1Copy_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjChild1Copy(pObj); }
Aig_Obj_t * Aig_ObjChild0Next_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjChild0Next(pObj); }
Aig_Obj_t * Aig_ObjChild1Next_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjChild1Next(pObj); }
void Aig_ObjChild0Flip_imctk_abc_sys(Aig_Obj_t *pObj) { Aig_ObjChild0Flip(pObj); }
void Aig_ObjChild1Flip_imctk_abc_sys(Aig_Obj_t *pObj) { Aig_ObjChild1Flip(pObj); }
Aig_Obj_t * Aig_ObjCopy_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjCopy(pObj); }
void Aig_ObjSetCopy_imctk_abc_sys(Aig_Obj_t *pObj, Aig_Obj_t *pCopy) { Aig_ObjSetCopy(pObj, pCopy); }
Aig_Obj_t * Aig_ObjRealCopy_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjRealCopy(pObj); }
int Aig_ObjToLit_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjToLit(pObj); }
Aig_Obj_t * Aig_ObjFromLit_imctk_abc_sys(Aig_Man_t *p, int iLit) { return Aig_ObjFromLit(p, iLit); }
int Aig_ObjLevel_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjLevel(pObj); }
int Aig_ObjLevelNew_imctk_abc_sys(Aig_Obj_t *pObj) { return Aig_ObjLevelNew(pObj); }
int Aig_ObjSetLevel_imctk_abc_sys(Aig_Obj_t *pObj, int i) { return Aig_ObjSetLevel(pObj, i); }
void Aig_ObjClean_imctk_abc_sys(Aig_Obj_t *pObj) { Aig_ObjClean(pObj); }
Aig_Obj_t * Aig_ObjFanout0_imctk_abc_sys(Aig_Man_t *p, Aig_Obj_t *pObj) { return Aig_ObjFanout0(p, pObj); }
Aig_Obj_t * Aig_ObjEquiv_imctk_abc_sys(Aig_Man_t *p, Aig_Obj_t *pObj) { return Aig_ObjEquiv(p, pObj); }
void Aig_ObjSetEquiv_imctk_abc_sys(Aig_Man_t *p, Aig_Obj_t *pObj, Aig_Obj_t *pEqu) { Aig_ObjSetEquiv(p, pObj, pEqu); }
Aig_Obj_t * Aig_ObjRepr_imctk_abc_sys(Aig_Man_t *p, Aig_Obj_t *pObj) { return Aig_ObjRepr(p, pObj); }
void Aig_ObjSetRepr_imctk_abc_sys(Aig_Man_t *p, Aig_Obj_t *pObj, Aig_Obj_t *pRepr) { Aig_ObjSetRepr(p, pObj, pRepr); }
int Aig_ObjWhatFanin_imctk_abc_sys(Aig_Obj_t *pObj, Aig_Obj_t *pFanin) { return Aig_ObjWhatFanin(pObj, pFanin); }
int Aig_ObjFanoutC_imctk_abc_sys(Aig_Obj_t *pObj, Aig_Obj_t *pFanout) { return Aig_ObjFanoutC(pObj, pFanout); }
Aig_Obj_t * Aig_ObjCreateGhost_imctk_abc_sys(Aig_Man_t *p, Aig_Obj_t *p0, Aig_Obj_t *p1, Aig_Type_t Type) { return Aig_ObjCreateGhost(p, p0, p1, Type); }
Aig_Obj_t * Aig_ManFetchMemory_imctk_abc_sys(Aig_Man_t *p) { return Aig_ManFetchMemory(p); }
void Aig_ManRecycleMemory_imctk_abc_sys(Aig_Man_t *p, Aig_Obj_t *pEntry) { Aig_ManRecycleMemory(p, pEntry); }
int Aig_ObjFanout0Int_imctk_abc_sys(Aig_Man_t *p, int ObjId) { return Aig_ObjFanout0Int(p, ObjId); }
int Aig_ObjFanoutNext_imctk_abc_sys(Aig_Man_t *p, int iFan) { return Aig_ObjFanoutNext(p, iFan); }
int Hop_BitWordNum_imctk_abc_sys(int nBits) { return Hop_BitWordNum(nBits); }
int Hop_TruthWordNum_imctk_abc_sys(int nVars) { return Hop_TruthWordNum(nVars); }
int Hop_InfoHasBit_imctk_abc_sys(unsigned int *p, int i) { return Hop_InfoHasBit(p, i); }
void Hop_InfoSetBit_imctk_abc_sys(unsigned int *p, int i) { Hop_InfoSetBit(p, i); }
void Hop_InfoXorBit_imctk_abc_sys(unsigned int *p, int i) { Hop_InfoXorBit(p, i); }
int Hop_Base2Log_imctk_abc_sys(unsigned int n) { return Hop_Base2Log(n); }
int Hop_Base10Log_imctk_abc_sys(unsigned int n) { return Hop_Base10Log(n); }
Hop_Obj_t * Hop_Regular_imctk_abc_sys(Hop_Obj_t *p) { return Hop_Regular(p); }
Hop_Obj_t * Hop_Not_imctk_abc_sys(Hop_Obj_t *p) { return Hop_Not(p); }
Hop_Obj_t * Hop_NotCond_imctk_abc_sys(Hop_Obj_t *p, int c) { return Hop_NotCond(p, c); }
int Hop_IsComplement_imctk_abc_sys(Hop_Obj_t *p) { return Hop_IsComplement(p); }
Hop_Obj_t * Hop_ManConst0_imctk_abc_sys(Hop_Man_t *p) { return Hop_ManConst0(p); }
Hop_Obj_t * Hop_ManConst1_imctk_abc_sys(Hop_Man_t *p) { return Hop_ManConst1(p); }
Hop_Obj_t * Hop_ManGhost_imctk_abc_sys(Hop_Man_t *p) { return Hop_ManGhost(p); }
Hop_Obj_t * Hop_ManPi_imctk_abc_sys(Hop_Man_t *p, int i) { return Hop_ManPi(p, i); }
Hop_Obj_t * Hop_ManPo_imctk_abc_sys(Hop_Man_t *p, int i) { return Hop_ManPo(p, i); }
Hop_Obj_t * Hop_ManObj_imctk_abc_sys(Hop_Man_t *p, int i) { return Hop_ManObj(p, i); }
Hop_Edge_t Hop_EdgeCreate_imctk_abc_sys(int Id, int fCompl) { return Hop_EdgeCreate(Id, fCompl); }
int Hop_EdgeId_imctk_abc_sys(Hop_Edge_t Edge) { return Hop_EdgeId(Edge); }
int Hop_EdgeIsComplement_imctk_abc_sys(Hop_Edge_t Edge) { return Hop_EdgeIsComplement(Edge); }
Hop_Edge_t Hop_EdgeRegular_imctk_abc_sys(Hop_Edge_t Edge) { return Hop_EdgeRegular(Edge); }
Hop_Edge_t Hop_EdgeNot_imctk_abc_sys(Hop_Edge_t Edge) { return Hop_EdgeNot(Edge); }
Hop_Edge_t Hop_EdgeNotCond_imctk_abc_sys(Hop_Edge_t Edge, int fCond) { return Hop_EdgeNotCond(Edge, fCond); }
int Hop_ManPiNum_imctk_abc_sys(Hop_Man_t *p) { return Hop_ManPiNum(p); }
int Hop_ManPoNum_imctk_abc_sys(Hop_Man_t *p) { return Hop_ManPoNum(p); }
int Hop_ManAndNum_imctk_abc_sys(Hop_Man_t *p) { return Hop_ManAndNum(p); }
int Hop_ManExorNum_imctk_abc_sys(Hop_Man_t *p) { return Hop_ManExorNum(p); }
int Hop_ManNodeNum_imctk_abc_sys(Hop_Man_t *p) { return Hop_ManNodeNum(p); }
int Hop_ManGetCost_imctk_abc_sys(Hop_Man_t *p) { return Hop_ManGetCost(p); }
int Hop_ManObjNum_imctk_abc_sys(Hop_Man_t *p) { return Hop_ManObjNum(p); }
Hop_Type_t Hop_ObjType_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjType(pObj); }
int Hop_ObjIsNone_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjIsNone(pObj); }
int Hop_ObjIsConst1_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjIsConst1(pObj); }
int Hop_ObjIsPi_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjIsPi(pObj); }
int Hop_ObjIsPo_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjIsPo(pObj); }
int Hop_ObjIsAnd_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjIsAnd(pObj); }
int Hop_ObjIsExor_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjIsExor(pObj); }
int Hop_ObjIsNode_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjIsNode(pObj); }
int Hop_ObjIsTerm_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjIsTerm(pObj); }
int Hop_ObjIsHash_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjIsHash(pObj); }
int Hop_ObjIsMarkA_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjIsMarkA(pObj); }
void Hop_ObjSetMarkA_imctk_abc_sys(Hop_Obj_t *pObj) { Hop_ObjSetMarkA(pObj); }
void Hop_ObjClearMarkA_imctk_abc_sys(Hop_Obj_t *pObj) { Hop_ObjClearMarkA(pObj); }
void Hop_ObjSetTravId_imctk_abc_sys(Hop_Obj_t *pObj, int TravId) { Hop_ObjSetTravId(pObj, TravId); }
void Hop_ObjSetTravIdCurrent_imctk_abc_sys(Hop_Man_t *p, Hop_Obj_t *pObj) { Hop_ObjSetTravIdCurrent(p, pObj); }
void Hop_ObjSetTravIdPrevious_imctk_abc_sys(Hop_Man_t *p, Hop_Obj_t *pObj) { Hop_ObjSetTravIdPrevious(p, pObj); }
int Hop_ObjIsTravIdCurrent_imctk_abc_sys(Hop_Man_t *p, Hop_Obj_t *pObj) { return Hop_ObjIsTravIdCurrent(p, pObj); }
int Hop_ObjIsTravIdPrevious_imctk_abc_sys(Hop_Man_t *p, Hop_Obj_t *pObj) { return Hop_ObjIsTravIdPrevious(p, pObj); }
int Hop_ObjTravId_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjTravId(pObj); }
int Hop_ObjPhase_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjPhase(pObj); }
int Hop_ObjRefs_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjRefs(pObj); }
void Hop_ObjRef_imctk_abc_sys(Hop_Obj_t *pObj) { Hop_ObjRef(pObj); }
void Hop_ObjDeref_imctk_abc_sys(Hop_Obj_t *pObj) { Hop_ObjDeref(pObj); }
void Hop_ObjClearRef_imctk_abc_sys(Hop_Obj_t *pObj) { Hop_ObjClearRef(pObj); }
int Hop_ObjFaninC0_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjFaninC0(pObj); }
int Hop_ObjFaninC1_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjFaninC1(pObj); }
Hop_Obj_t * Hop_ObjFanin0_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjFanin0(pObj); }
Hop_Obj_t * Hop_ObjFanin1_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjFanin1(pObj); }
Hop_Obj_t * Hop_ObjChild0_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjChild0(pObj); }
Hop_Obj_t * Hop_ObjChild1_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjChild1(pObj); }
Hop_Obj_t * Hop_ObjChild0Copy_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjChild0Copy(pObj); }
Hop_Obj_t * Hop_ObjChild1Copy_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjChild1Copy(pObj); }
int Hop_ObjChild0CopyI_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjChild0CopyI(pObj); }
int Hop_ObjChild1CopyI_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjChild1CopyI(pObj); }
int Hop_ObjLevel_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjLevel(pObj); }
int Hop_ObjLevelNew_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjLevelNew(pObj); }
int Hop_ObjPhaseCompl_imctk_abc_sys(Hop_Obj_t *pObj) { return Hop_ObjPhaseCompl(pObj); }
void Hop_ObjClean_imctk_abc_sys(Hop_Obj_t *pObj) { Hop_ObjClean(pObj); }
int Hop_ObjWhatFanin_imctk_abc_sys(Hop_Obj_t *pObj, Hop_Obj_t *pFanin) { return Hop_ObjWhatFanin(pObj, pFanin); }
int Hop_ObjFanoutC_imctk_abc_sys(Hop_Obj_t *pObj, Hop_Obj_t *pFanout) { return Hop_ObjFanoutC(pObj, pFanout); }
Hop_Obj_t * Hop_ObjCreateGhost_imctk_abc_sys(Hop_Man_t *p, Hop_Obj_t *p0, Hop_Obj_t *p1, Hop_Type_t Type) { return Hop_ObjCreateGhost(p, p0, p1, Type); }
Hop_Obj_t * Hop_ManFetchMemory_imctk_abc_sys(Hop_Man_t *p) { return Hop_ManFetchMemory(p); }
void Hop_ManRecycleMemory_imctk_abc_sys(Hop_Man_t *p, Hop_Obj_t *pEntry) { Hop_ManRecycleMemory(p, pEntry); }
void Extra_ProgressBarUpdate_imctk_abc_sys(ProgressBar *p, int nItemsCur, char *pString) { Extra_ProgressBarUpdate(p, nItemsCur, pString); }
int Extra_BitWordNum_imctk_abc_sys(int nBits) { return Extra_BitWordNum(nBits); }
int Extra_TruthWordNum_imctk_abc_sys(int nVars) { return Extra_TruthWordNum(nVars); }
void Extra_TruthSetBit_imctk_abc_sys(unsigned int *p, int Bit) { Extra_TruthSetBit(p, Bit); }
void Extra_TruthXorBit_imctk_abc_sys(unsigned int *p, int Bit) { Extra_TruthXorBit(p, Bit); }
int Extra_TruthHasBit_imctk_abc_sys(unsigned int *p, int Bit) { return Extra_TruthHasBit(p, Bit); }
int Extra_WordCountOnes_imctk_abc_sys(unsigned int uWord) { return Extra_WordCountOnes(uWord); }
int Extra_TruthCountOnes_imctk_abc_sys(unsigned int *pIn, int nVars) { return Extra_TruthCountOnes(pIn, nVars); }
int Extra_TruthIsEqual_imctk_abc_sys(unsigned int *pIn0, unsigned int *pIn1, int nVars) { return Extra_TruthIsEqual(pIn0, pIn1, nVars); }
int Extra_TruthIsConst0_imctk_abc_sys(unsigned int *pIn, int nVars) { return Extra_TruthIsConst0(pIn, nVars); }
int Extra_TruthIsConst1_imctk_abc_sys(unsigned int *pIn, int nVars) { return Extra_TruthIsConst1(pIn, nVars); }
int Extra_TruthIsImply_imctk_abc_sys(unsigned int *pIn1, unsigned int *pIn2, int nVars) { return Extra_TruthIsImply(pIn1, pIn2, nVars); }
void Extra_TruthCopy_imctk_abc_sys(unsigned int *pOut, unsigned int *pIn, int nVars) { Extra_TruthCopy(pOut, pIn, nVars); }
void Extra_TruthClear_imctk_abc_sys(unsigned int *pOut, int nVars) { Extra_TruthClear(pOut, nVars); }
void Extra_TruthFill_imctk_abc_sys(unsigned int *pOut, int nVars) { Extra_TruthFill(pOut, nVars); }
void Extra_TruthNot_imctk_abc_sys(unsigned int *pOut, unsigned int *pIn, int nVars) { Extra_TruthNot(pOut, pIn, nVars); }
void Extra_TruthAnd_imctk_abc_sys(unsigned int *pOut, unsigned int *pIn0, unsigned int *pIn1, int nVars) { Extra_TruthAnd(pOut, pIn0, pIn1, nVars); }
void Extra_TruthOr_imctk_abc_sys(unsigned int *pOut, unsigned int *pIn0, unsigned int *pIn1, int nVars) { Extra_TruthOr(pOut, pIn0, pIn1, nVars); }
void Extra_TruthSharp_imctk_abc_sys(unsigned int *pOut, unsigned int *pIn0, unsigned int *pIn1, int nVars) { Extra_TruthSharp(pOut, pIn0, pIn1, nVars); }
void Extra_TruthNand_imctk_abc_sys(unsigned int *pOut, unsigned int *pIn0, unsigned int *pIn1, int nVars) { Extra_TruthNand(pOut, pIn0, pIn1, nVars); }
void Extra_TruthAndPhase_imctk_abc_sys(unsigned int *pOut, unsigned int *pIn0, unsigned int *pIn1, int nVars, int fCompl0, int fCompl1) { Extra_TruthAndPhase(pOut, pIn0, pIn1, nVars, fCompl0, fCompl1); }
unsigned int Abc_InfoRandomWord_imctk_abc_sys(void) { return Abc_InfoRandomWord(); }
void Abc_InfoRandom_imctk_abc_sys(unsigned int *p, int nWords) { Abc_InfoRandom(p, nWords); }
void Abc_InfoClear_imctk_abc_sys(unsigned int *p, int nWords) { Abc_InfoClear(p, nWords); }
void Abc_InfoFill_imctk_abc_sys(unsigned int *p, int nWords) { Abc_InfoFill(p, nWords); }
void Abc_InfoNot_imctk_abc_sys(unsigned int *p, int nWords) { Abc_InfoNot(p, nWords); }
int Abc_InfoIsZero_imctk_abc_sys(unsigned int *p, int nWords) { return Abc_InfoIsZero(p, nWords); }
int Abc_InfoIsOne_imctk_abc_sys(unsigned int *p, int nWords) { return Abc_InfoIsOne(p, nWords); }
void Abc_InfoCopy_imctk_abc_sys(unsigned int *p, unsigned int *q, int nWords) { Abc_InfoCopy(p, q, nWords); }
void Abc_InfoAnd_imctk_abc_sys(unsigned int *p, unsigned int *q, int nWords) { Abc_InfoAnd(p, q, nWords); }
void Abc_InfoOr_imctk_abc_sys(unsigned int *p, unsigned int *q, int nWords) { Abc_InfoOr(p, q, nWords); }
void Abc_InfoXor_imctk_abc_sys(unsigned int *p, unsigned int *q, int nWords) { Abc_InfoXor(p, q, nWords); }
int Abc_InfoIsOrOne_imctk_abc_sys(unsigned int *p, unsigned int *q, int nWords) { return Abc_InfoIsOrOne(p, q, nWords); }
int Abc_InfoIsOrOne3_imctk_abc_sys(unsigned int *p, unsigned int *q, unsigned int *r, int nWords) { return Abc_InfoIsOrOne3(p, q, r, nWords); }
int Abc_NtkIsNetlist_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkIsNetlist(pNtk); }
int Abc_NtkIsLogic_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkIsLogic(pNtk); }
int Abc_NtkIsStrash_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkIsStrash(pNtk); }
int Abc_NtkHasSop_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkHasSop(pNtk); }
int Abc_NtkHasBdd_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkHasBdd(pNtk); }
int Abc_NtkHasAig_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkHasAig(pNtk); }
int Abc_NtkHasMapping_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkHasMapping(pNtk); }
int Abc_NtkHasBlifMv_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkHasBlifMv(pNtk); }
int Abc_NtkHasBlackbox_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkHasBlackbox(pNtk); }
int Abc_NtkIsSopNetlist_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkIsSopNetlist(pNtk); }
int Abc_NtkIsBddNetlist_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkIsBddNetlist(pNtk); }
int Abc_NtkIsAigNetlist_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkIsAigNetlist(pNtk); }
int Abc_NtkIsMappedNetlist_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkIsMappedNetlist(pNtk); }
int Abc_NtkIsBlifMvNetlist_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkIsBlifMvNetlist(pNtk); }
int Abc_NtkIsSopLogic_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkIsSopLogic(pNtk); }
int Abc_NtkIsBddLogic_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkIsBddLogic(pNtk); }
int Abc_NtkIsAigLogic_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkIsAigLogic(pNtk); }
int Abc_NtkIsMappedLogic_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkIsMappedLogic(pNtk); }
char * Abc_NtkName_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkName(pNtk); }
char * Abc_NtkSpec_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkSpec(pNtk); }
Abc_Ntk_t * Abc_NtkExdc_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkExdc(pNtk); }
Abc_Ntk_t * Abc_NtkBackup_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkBackup(pNtk); }
int Abc_NtkStep_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkStep(pNtk); }
void Abc_NtkSetName_imctk_abc_sys(Abc_Ntk_t *pNtk, char *pName) { Abc_NtkSetName(pNtk, pName); }
void Abc_NtkSetSpec_imctk_abc_sys(Abc_Ntk_t *pNtk, char *pName) { Abc_NtkSetSpec(pNtk, pName); }
void Abc_NtkSetBackup_imctk_abc_sys(Abc_Ntk_t *pNtk, Abc_Ntk_t *pNetBackup) { Abc_NtkSetBackup(pNtk, pNetBackup); }
void Abc_NtkSetStep_imctk_abc_sys(Abc_Ntk_t *pNtk, int iStep) { Abc_NtkSetStep(pNtk, iStep); }
int Abc_NtkObjNum_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkObjNum(pNtk); }
int Abc_NtkObjNumMax_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkObjNumMax(pNtk); }
int Abc_NtkPiNum_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkPiNum(pNtk); }
int Abc_NtkPoNum_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkPoNum(pNtk); }
int Abc_NtkCiNum_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkCiNum(pNtk); }
int Abc_NtkCoNum_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkCoNum(pNtk); }
int Abc_NtkBoxNum_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkBoxNum(pNtk); }
int Abc_NtkBiNum_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkBiNum(pNtk); }
int Abc_NtkBoNum_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkBoNum(pNtk); }
int Abc_NtkNetNum_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkNetNum(pNtk); }
int Abc_NtkNodeNum_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkNodeNum(pNtk); }
int Abc_NtkLatchNum_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkLatchNum(pNtk); }
int Abc_NtkWhiteboxNum_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkWhiteboxNum(pNtk); }
int Abc_NtkBlackboxNum_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkBlackboxNum(pNtk); }
int Abc_NtkIsComb_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkIsComb(pNtk); }
int Abc_NtkHasOnlyLatchBoxes_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkHasOnlyLatchBoxes(pNtk); }
int Abc_NtkConstrNum_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkConstrNum(pNtk); }
Abc_Obj_t * Abc_NtkCreatePi_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkCreatePi(pNtk); }
Abc_Obj_t * Abc_NtkCreatePo_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkCreatePo(pNtk); }
Abc_Obj_t * Abc_NtkCreateBi_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkCreateBi(pNtk); }
Abc_Obj_t * Abc_NtkCreateBo_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkCreateBo(pNtk); }
Abc_Obj_t * Abc_NtkCreateNet_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkCreateNet(pNtk); }
Abc_Obj_t * Abc_NtkCreateNode_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkCreateNode(pNtk); }
Abc_Obj_t * Abc_NtkCreateLatch_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkCreateLatch(pNtk); }
Abc_Obj_t * Abc_NtkCreateWhitebox_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkCreateWhitebox(pNtk); }
Abc_Obj_t * Abc_NtkCreateBlackbox_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkCreateBlackbox(pNtk); }
Abc_Obj_t * Abc_NtkObj_imctk_abc_sys(Abc_Ntk_t *pNtk, int i) { return Abc_NtkObj(pNtk, i); }
Abc_Obj_t * Abc_NtkPi_imctk_abc_sys(Abc_Ntk_t *pNtk, int i) { return Abc_NtkPi(pNtk, i); }
Abc_Obj_t * Abc_NtkPo_imctk_abc_sys(Abc_Ntk_t *pNtk, int i) { return Abc_NtkPo(pNtk, i); }
Abc_Obj_t * Abc_NtkCi_imctk_abc_sys(Abc_Ntk_t *pNtk, int i) { return Abc_NtkCi(pNtk, i); }
Abc_Obj_t * Abc_NtkCo_imctk_abc_sys(Abc_Ntk_t *pNtk, int i) { return Abc_NtkCo(pNtk, i); }
Abc_Obj_t * Abc_NtkBox_imctk_abc_sys(Abc_Ntk_t *pNtk, int i) { return Abc_NtkBox(pNtk, i); }
int Abc_ObjIsComplement_imctk_abc_sys(Abc_Obj_t *p) { return Abc_ObjIsComplement(p); }
Abc_Obj_t * Abc_ObjRegular_imctk_abc_sys(Abc_Obj_t *p) { return Abc_ObjRegular(p); }
Abc_Obj_t * Abc_ObjNot_imctk_abc_sys(Abc_Obj_t *p) { return Abc_ObjNot(p); }
Abc_Obj_t * Abc_ObjNotCond_imctk_abc_sys(Abc_Obj_t *p, int c) { return Abc_ObjNotCond(p, c); }
unsigned int Abc_ObjType_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjType(pObj); }
unsigned int Abc_ObjId_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjId(pObj); }
int Abc_ObjLevel_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjLevel(pObj); }
Vec_Int_t * Abc_ObjFaninVec_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjFaninVec(pObj); }
Vec_Int_t * Abc_ObjFanoutVec_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjFanoutVec(pObj); }
Abc_Obj_t * Abc_ObjCopy_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjCopy(pObj); }
Abc_Ntk_t * Abc_ObjNtk_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjNtk(pObj); }
Abc_Ntk_t * Abc_ObjModel_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjModel(pObj); }
void * Abc_ObjData_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjData(pObj); }
Abc_Obj_t * Abc_ObjEquiv_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjEquiv(pObj); }
Abc_Obj_t * Abc_ObjCopyCond_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjCopyCond(pObj); }
void Abc_ObjSetLevel_imctk_abc_sys(Abc_Obj_t *pObj, int Level) { Abc_ObjSetLevel(pObj, Level); }
void Abc_ObjSetCopy_imctk_abc_sys(Abc_Obj_t *pObj, Abc_Obj_t *pCopy) { Abc_ObjSetCopy(pObj, pCopy); }
void Abc_ObjSetData_imctk_abc_sys(Abc_Obj_t *pObj, void *pData) { Abc_ObjSetData(pObj, pData); }
int Abc_ObjIsNone_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjIsNone(pObj); }
int Abc_ObjIsPi_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjIsPi(pObj); }
int Abc_ObjIsPo_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjIsPo(pObj); }
int Abc_ObjIsBi_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjIsBi(pObj); }
int Abc_ObjIsBo_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjIsBo(pObj); }
int Abc_ObjIsCi_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjIsCi(pObj); }
int Abc_ObjIsCo_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjIsCo(pObj); }
int Abc_ObjIsTerm_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjIsTerm(pObj); }
int Abc_ObjIsNet_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjIsNet(pObj); }
int Abc_ObjIsNode_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjIsNode(pObj); }
int Abc_ObjIsLatch_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjIsLatch(pObj); }
int Abc_ObjIsBox_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjIsBox(pObj); }
int Abc_ObjIsWhitebox_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjIsWhitebox(pObj); }
int Abc_ObjIsBlackbox_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjIsBlackbox(pObj); }
int Abc_ObjIsBarBuf_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjIsBarBuf(pObj); }
void Abc_ObjBlackboxToWhitebox_imctk_abc_sys(Abc_Obj_t *pObj) { Abc_ObjBlackboxToWhitebox(pObj); }
int Abc_ObjFaninNum_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjFaninNum(pObj); }
int Abc_ObjFanoutNum_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjFanoutNum(pObj); }
int Abc_ObjFaninId_imctk_abc_sys(Abc_Obj_t *pObj, int i) { return Abc_ObjFaninId(pObj, i); }
int Abc_ObjFaninId0_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjFaninId0(pObj); }
int Abc_ObjFaninId1_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjFaninId1(pObj); }
int Abc_ObjFanoutEdgeNum_imctk_abc_sys(Abc_Obj_t *pObj, Abc_Obj_t *pFanout) { return Abc_ObjFanoutEdgeNum(pObj, pFanout); }
Abc_Obj_t * Abc_ObjFanout_imctk_abc_sys(Abc_Obj_t *pObj, int i) { return Abc_ObjFanout(pObj, i); }
Abc_Obj_t * Abc_ObjFanout0_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjFanout0(pObj); }
Abc_Obj_t * Abc_ObjFanin_imctk_abc_sys(Abc_Obj_t *pObj, int i) { return Abc_ObjFanin(pObj, i); }
Abc_Obj_t * Abc_ObjFanin0_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjFanin0(pObj); }
Abc_Obj_t * Abc_ObjFanin1_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjFanin1(pObj); }
Abc_Obj_t * Abc_ObjFanin0Ntk_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjFanin0Ntk(pObj); }
Abc_Obj_t * Abc_ObjFanout0Ntk_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjFanout0Ntk(pObj); }
int Abc_ObjFaninC0_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjFaninC0(pObj); }
int Abc_ObjFaninC1_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjFaninC1(pObj); }
int Abc_ObjFaninC_imctk_abc_sys(Abc_Obj_t *pObj, int i) { return Abc_ObjFaninC(pObj, i); }
void Abc_ObjSetFaninC_imctk_abc_sys(Abc_Obj_t *pObj, int i) { Abc_ObjSetFaninC(pObj, i); }
void Abc_ObjXorFaninC_imctk_abc_sys(Abc_Obj_t *pObj, int i) { Abc_ObjXorFaninC(pObj, i); }
Abc_Obj_t * Abc_ObjChild_imctk_abc_sys(Abc_Obj_t *pObj, int i) { return Abc_ObjChild(pObj, i); }
Abc_Obj_t * Abc_ObjChild0_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjChild0(pObj); }
Abc_Obj_t * Abc_ObjChild1_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjChild1(pObj); }
Abc_Obj_t * Abc_ObjChildCopy_imctk_abc_sys(Abc_Obj_t *pObj, int i) { return Abc_ObjChildCopy(pObj, i); }
Abc_Obj_t * Abc_ObjChild0Copy_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjChild0Copy(pObj); }
Abc_Obj_t * Abc_ObjChild1Copy_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjChild1Copy(pObj); }
Abc_Obj_t * Abc_ObjChild0Data_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjChild0Data(pObj); }
Abc_Obj_t * Abc_ObjChild1Data_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjChild1Data(pObj); }
Abc_Obj_t * Abc_ObjFromLit_imctk_abc_sys(Abc_Ntk_t *p, int iLit) { return Abc_ObjFromLit(p, iLit); }
int Abc_ObjToLit_imctk_abc_sys(Abc_Obj_t *p) { return Abc_ObjToLit(p); }
int Abc_ObjFaninPhase_imctk_abc_sys(Abc_Obj_t *p, int i) { return Abc_ObjFaninPhase(p, i); }
void Abc_ObjFaninFlipPhase_imctk_abc_sys(Abc_Obj_t *p, int i) { Abc_ObjFaninFlipPhase(p, i); }
int Abc_AigNodeIsConst_imctk_abc_sys(Abc_Obj_t *pNode) { return Abc_AigNodeIsConst(pNode); }
int Abc_AigNodeIsAnd_imctk_abc_sys(Abc_Obj_t *pNode) { return Abc_AigNodeIsAnd(pNode); }
int Abc_AigNodeIsChoice_imctk_abc_sys(Abc_Obj_t *pNode) { return Abc_AigNodeIsChoice(pNode); }
int Abc_NodeIsPersistant_imctk_abc_sys(Abc_Obj_t *pNode) { return Abc_NodeIsPersistant(pNode); }
void Abc_NodeSetPersistant_imctk_abc_sys(Abc_Obj_t *pNode) { Abc_NodeSetPersistant(pNode); }
void Abc_NodeClearPersistant_imctk_abc_sys(Abc_Obj_t *pNode) { Abc_NodeClearPersistant(pNode); }
void Abc_NtkIncrementTravId_imctk_abc_sys(Abc_Ntk_t *p) { Abc_NtkIncrementTravId(p); }
int Abc_NodeTravId_imctk_abc_sys(Abc_Obj_t *p) { return Abc_NodeTravId(p); }
void Abc_NodeSetTravId_imctk_abc_sys(Abc_Obj_t *p, int TravId) { Abc_NodeSetTravId(p, TravId); }
void Abc_NodeSetTravIdCurrent_imctk_abc_sys(Abc_Obj_t *p) { Abc_NodeSetTravIdCurrent(p); }
void Abc_NodeSetTravIdPrevious_imctk_abc_sys(Abc_Obj_t *p) { Abc_NodeSetTravIdPrevious(p); }
int Abc_NodeIsTravIdCurrent_imctk_abc_sys(Abc_Obj_t *p) { return Abc_NodeIsTravIdCurrent(p); }
int Abc_NodeIsTravIdPrevious_imctk_abc_sys(Abc_Obj_t *p) { return Abc_NodeIsTravIdPrevious(p); }
void Abc_NodeSetTravIdCurrentId_imctk_abc_sys(Abc_Ntk_t *p, int i) { Abc_NodeSetTravIdCurrentId(p, i); }
int Abc_NodeIsTravIdCurrentId_imctk_abc_sys(Abc_Ntk_t *p, int i) { return Abc_NodeIsTravIdCurrentId(p, i); }
void Abc_LatchSetInitNone_imctk_abc_sys(Abc_Obj_t *pLatch) { Abc_LatchSetInitNone(pLatch); }
void Abc_LatchSetInit0_imctk_abc_sys(Abc_Obj_t *pLatch) { Abc_LatchSetInit0(pLatch); }
void Abc_LatchSetInit1_imctk_abc_sys(Abc_Obj_t *pLatch) { Abc_LatchSetInit1(pLatch); }
void Abc_LatchSetInitDc_imctk_abc_sys(Abc_Obj_t *pLatch) { Abc_LatchSetInitDc(pLatch); }
int Abc_LatchIsInitNone_imctk_abc_sys(Abc_Obj_t *pLatch) { return Abc_LatchIsInitNone(pLatch); }
int Abc_LatchIsInit0_imctk_abc_sys(Abc_Obj_t *pLatch) { return Abc_LatchIsInit0(pLatch); }
int Abc_LatchIsInit1_imctk_abc_sys(Abc_Obj_t *pLatch) { return Abc_LatchIsInit1(pLatch); }
int Abc_LatchIsInitDc_imctk_abc_sys(Abc_Obj_t *pLatch) { return Abc_LatchIsInitDc(pLatch); }
int Abc_LatchInit_imctk_abc_sys(Abc_Obj_t *pLatch) { return Abc_LatchInit(pLatch); }
void * Abc_NtkGlobalBdd_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkGlobalBdd(pNtk); }
void * Abc_NtkGlobalBddMan_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkGlobalBddMan(pNtk); }
void ** Abc_NtkGlobalBddArray_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkGlobalBddArray(pNtk); }
void * Abc_ObjGlobalBdd_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjGlobalBdd(pObj); }
void Abc_ObjSetGlobalBdd_imctk_abc_sys(Abc_Obj_t *pObj, void *bF) { Abc_ObjSetGlobalBdd(pObj, bF); }
void * Abc_NtkMvVar_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkMvVar(pNtk); }
void * Abc_NtkMvVarMan_imctk_abc_sys(Abc_Ntk_t *pNtk) { return Abc_NtkMvVarMan(pNtk); }
void * Abc_ObjMvVar_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjMvVar(pObj); }
int Abc_ObjMvVarNum_imctk_abc_sys(Abc_Obj_t *pObj) { return Abc_ObjMvVarNum(pObj); }
void Abc_ObjSetMvVar_imctk_abc_sys(Abc_Obj_t *pObj, void *pV) { Abc_ObjSetMvVar(pObj, pV); }
int Saig_ManPiNum_imctk_abc_sys(Aig_Man_t *p) { return Saig_ManPiNum(p); }
int Saig_ManPoNum_imctk_abc_sys(Aig_Man_t *p) { return Saig_ManPoNum(p); }
int Saig_ManCiNum_imctk_abc_sys(Aig_Man_t *p) { return Saig_ManCiNum(p); }
int Saig_ManCoNum_imctk_abc_sys(Aig_Man_t *p) { return Saig_ManCoNum(p); }
int Saig_ManRegNum_imctk_abc_sys(Aig_Man_t *p) { return Saig_ManRegNum(p); }
int Saig_ManConstrNum_imctk_abc_sys(Aig_Man_t *p) { return Saig_ManConstrNum(p); }
Aig_Obj_t * Saig_ManLo_imctk_abc_sys(Aig_Man_t *p, int i) { return Saig_ManLo(p, i); }
Aig_Obj_t * Saig_ManLi_imctk_abc_sys(Aig_Man_t *p, int i) { return Saig_ManLi(p, i); }
int Saig_ObjIsPi_imctk_abc_sys(Aig_Man_t *p, Aig_Obj_t *pObj) { return Saig_ObjIsPi(p, pObj); }
int Saig_ObjIsPo_imctk_abc_sys(Aig_Man_t *p, Aig_Obj_t *pObj) { return Saig_ObjIsPo(p, pObj); }
int Saig_ObjIsLo_imctk_abc_sys(Aig_Man_t *p, Aig_Obj_t *pObj) { return Saig_ObjIsLo(p, pObj); }
int Saig_ObjIsLi_imctk_abc_sys(Aig_Man_t *p, Aig_Obj_t *pObj) { return Saig_ObjIsLi(p, pObj); }
Aig_Obj_t * Saig_ObjLoToLi_imctk_abc_sys(Aig_Man_t *p, Aig_Obj_t *pObj) { return Saig_ObjLoToLi(p, pObj); }
Aig_Obj_t * Saig_ObjLiToLo_imctk_abc_sys(Aig_Man_t *p, Aig_Obj_t *pObj) { return Saig_ObjLiToLo(p, pObj); }
int Saig_ObjRegId_imctk_abc_sys(Aig_Man_t *p, Aig_Obj_t *pObj) { return Saig_ObjRegId(p, pObj); }
Dar_Cut_t * Dar_ObjCuts_imctk_abc_sys(Aig_Obj_t *pObj) { return Dar_ObjCuts(pObj); }
void Dar_ObjSetCuts_imctk_abc_sys(Aig_Obj_t *pObj, Dar_Cut_t *pCuts) { Dar_ObjSetCuts(pObj, pCuts); }
Dar_Cut_t * Dar_ObjBestCut_imctk_abc_sys(Aig_Obj_t *pObj) { return Dar_ObjBestCut(pObj); }
int Cnf_CutSopCost_imctk_abc_sys(Cnf_Man_t *p, Dar_Cut_t *pCut) { return Cnf_CutSopCost(p, pCut); }
int Cnf_CutLeaveNum_imctk_abc_sys(Cnf_Cut_t *pCut) { return Cnf_CutLeaveNum(pCut); }
int * Cnf_CutLeaves_imctk_abc_sys(Cnf_Cut_t *pCut) { return Cnf_CutLeaves(pCut); }
unsigned int * Cnf_CutTruth_imctk_abc_sys(Cnf_Cut_t *pCut) { return Cnf_CutTruth(pCut); }
Cnf_Cut_t * Cnf_ObjBestCut_imctk_abc_sys(Aig_Obj_t *pObj) { return Cnf_ObjBestCut(pObj); }
void Cnf_ObjSetBestCut_imctk_abc_sys(Aig_Obj_t *pObj, Cnf_Cut_t *pCut) { Cnf_ObjSetBestCut(pObj, pCut); }
Hash_Int_t * Hash_IntAlloc_imctk_abc_sys(int nBins) { return Hash_IntAlloc(nBins); }
int Hash_IntExists_imctk_abc_sys(Hash_Int_t *p, int key) { return Hash_IntExists(p, key); }
void Hash_IntWriteEntry_imctk_abc_sys(Hash_Int_t *p, int key, int data) { Hash_IntWriteEntry(p, key, data); }
int Hash_IntEntry_imctk_abc_sys(Hash_Int_t *p, int key, int fCreate) { return Hash_IntEntry(p, key, fCreate); }
int * Hash_IntEntryPtr_imctk_abc_sys(Hash_Int_t *p, int key) { return Hash_IntEntryPtr(p, key); }
void Hash_IntFree_imctk_abc_sys(Hash_Int_t *p) { Hash_IntFree(p); }
void veci_new_imctk_abc_sys(veci *v) { veci_new(v); }
void veci_delete_imctk_abc_sys(veci *v) { veci_delete(v); }
int * veci_begin_imctk_abc_sys(veci *v) { return veci_begin(v); }
int veci_size_imctk_abc_sys(veci *v) { return veci_size(v); }
void veci_resize_imctk_abc_sys(veci *v, int k) { veci_resize(v, k); }
int veci_pop_imctk_abc_sys(veci *v) { return veci_pop(v); }
void veci_push_imctk_abc_sys(veci *v, int e) { veci_push(v, e); }
void veci_remove_imctk_abc_sys(veci *v, int e) { veci_remove(v, e); }
void vecp_new_imctk_abc_sys(vecp *v) { vecp_new(v); }
void vecp_delete_imctk_abc_sys(vecp *v) { vecp_delete(v); }
void ** vecp_begin_imctk_abc_sys(vecp *v) { return vecp_begin(v); }
int vecp_size_imctk_abc_sys(vecp *v) { return vecp_size(v); }
void vecp_resize_imctk_abc_sys(vecp *v, int k) { vecp_resize(v, k); }
void vecp_push_imctk_abc_sys(vecp *v, void *e) { vecp_push(v, e); }
void vecp_remove_imctk_abc_sys(vecp *v, void *e) { vecp_remove(v, e); }
lit toLit_imctk_abc_sys(int v) { return toLit(v); }
lit toLitCond_imctk_abc_sys(int v, int c) { return toLitCond(v, c); }
lit lit_neg_imctk_abc_sys(lit l) { return lit_neg(l); }
int lit_var_imctk_abc_sys(lit l) { return lit_var(l); }
int lit_sign_imctk_abc_sys(lit l) { return lit_sign(l); }
int lit_print_imctk_abc_sys(lit l) { return lit_print(l); }
lit lit_read_imctk_abc_sys(int s) { return lit_read(s); }
int lit_check_imctk_abc_sys(lit l, int n) { return lit_check(l, n); }
int Sat_MemLimit_imctk_abc_sys(int *p) { return Sat_MemLimit(p); }
int Sat_MemIncLimit_imctk_abc_sys(int *p, int nInts) { return Sat_MemIncLimit(p, nInts); }
void Sat_MemWriteLimit_imctk_abc_sys(int *p, int nInts) { Sat_MemWriteLimit(p, nInts); }
int Sat_MemHandPage_imctk_abc_sys(Sat_Mem_t *p, cla h) { return Sat_MemHandPage(p, h); }
int Sat_MemHandShift_imctk_abc_sys(Sat_Mem_t *p, cla h) { return Sat_MemHandShift(p, h); }
int Sat_MemIntSize_imctk_abc_sys(int size, int lrn) { return Sat_MemIntSize(size, lrn); }
int Sat_MemClauseSize_imctk_abc_sys(clause *p) { return Sat_MemClauseSize(p); }
int Sat_MemClauseSize2_imctk_abc_sys(clause *p) { return Sat_MemClauseSize2(p); }
clause * Sat_MemClause_imctk_abc_sys(Sat_Mem_t *p, int i, int k) { return Sat_MemClause(p, i, k); }
clause * Sat_MemClauseHand_imctk_abc_sys(Sat_Mem_t *p, cla h) { return Sat_MemClauseHand(p, h); }
int Sat_MemEntryNum_imctk_abc_sys(Sat_Mem_t *p, int lrn) { return Sat_MemEntryNum(p, lrn); }
cla Sat_MemHand_imctk_abc_sys(Sat_Mem_t *p, int i, int k) { return Sat_MemHand(p, i, k); }
cla Sat_MemHandCurrent_imctk_abc_sys(Sat_Mem_t *p, int lrn) { return Sat_MemHandCurrent(p, lrn); }
int Sat_MemClauseUsed_imctk_abc_sys(Sat_Mem_t *p, cla h) { return Sat_MemClauseUsed(p, h); }
double Sat_MemMemoryHand_imctk_abc_sys(Sat_Mem_t *p, cla h) { return Sat_MemMemoryHand(p, h); }
double Sat_MemMemoryUsed_imctk_abc_sys(Sat_Mem_t *p, int lrn) { return Sat_MemMemoryUsed(p, lrn); }
double Sat_MemMemoryAllUsed_imctk_abc_sys(Sat_Mem_t *p) { return Sat_MemMemoryAllUsed(p); }
double Sat_MemMemoryAll_imctk_abc_sys(Sat_Mem_t *p) { return Sat_MemMemoryAll(p); }
int clause_from_lit_imctk_abc_sys(lit l) { return clause_from_lit(l); }
int clause_is_lit_imctk_abc_sys(cla h) { return clause_is_lit(h); }
lit clause_read_lit_imctk_abc_sys(cla h) { return clause_read_lit(h); }
int clause_learnt_h_imctk_abc_sys(Sat_Mem_t *p, cla h) { return clause_learnt_h(p, h); }
int clause_learnt_imctk_abc_sys(clause *c) { return clause_learnt(c); }
int clause_id_imctk_abc_sys(clause *c) { return clause_id(c); }
void clause_set_id_imctk_abc_sys(clause *c, int id) { clause_set_id(c, id); }
int clause_size_imctk_abc_sys(clause *c) { return clause_size(c); }
lit * clause_begin_imctk_abc_sys(clause *c) { return clause_begin(c); }
lit * clause_end_imctk_abc_sys(clause *c) { return clause_end(c); }
void clause_print__imctk_abc_sys(clause *c) { clause_print_(c); }
int Sat_MemCountL_imctk_abc_sys(Sat_Mem_t *p) { return Sat_MemCountL(p); }
void Sat_MemAlloc__imctk_abc_sys(Sat_Mem_t *p, int nPageSize) { Sat_MemAlloc_(p, nPageSize); }
Sat_Mem_t * Sat_MemAlloc_imctk_abc_sys(int nPageSize) { return Sat_MemAlloc(nPageSize); }
void Sat_MemRestart_imctk_abc_sys(Sat_Mem_t *p) { Sat_MemRestart(p); }
void Sat_MemBookMark_imctk_abc_sys(Sat_Mem_t *p) { Sat_MemBookMark(p); }
void Sat_MemRollBack_imctk_abc_sys(Sat_Mem_t *p) { Sat_MemRollBack(p); }
void Sat_MemFree__imctk_abc_sys(Sat_Mem_t *p) { Sat_MemFree_(p); }
void Sat_MemFree_imctk_abc_sys(Sat_Mem_t *p) { Sat_MemFree(p); }
int Sat_MemAppend_imctk_abc_sys(Sat_Mem_t *p, int *pArray, int nSize, int lrn, int fPlus1) { return Sat_MemAppend(p, pArray, nSize, lrn, fPlus1); }
void Sat_MemShrink_imctk_abc_sys(Sat_Mem_t *p, int h, int lrn) { Sat_MemShrink(p, h, lrn); }
int Sat_MemCompactLearned_imctk_abc_sys(Sat_Mem_t *p, int fDoMove) { return Sat_MemCompactLearned(p, fDoMove); }
word Xdbl_Exp_imctk_abc_sys(xdbl a) { return Xdbl_Exp(a); }
word Xdbl_Mnt_imctk_abc_sys(xdbl a) { return Xdbl_Mnt(a); }
xdbl Xdbl_Create_imctk_abc_sys(word Exp, word Mnt) { return Xdbl_Create(Exp, Mnt); }
xdbl Xdbl_Const1_imctk_abc_sys(void) { return Xdbl_Const1(); }
xdbl Xdbl_Const2_imctk_abc_sys(void) { return Xdbl_Const2(); }
xdbl Xdbl_Const3_imctk_abc_sys(void) { return Xdbl_Const3(); }
xdbl Xdbl_Const12_imctk_abc_sys(void) { return Xdbl_Const12(); }
xdbl Xdbl_Const1point5_imctk_abc_sys(void) { return Xdbl_Const1point5(); }
xdbl Xdbl_Const2point5_imctk_abc_sys(void) { return Xdbl_Const2point5(); }
xdbl Xdbl_Maximum_imctk_abc_sys(void) { return Xdbl_Maximum(); }
double Xdbl_ToDouble_imctk_abc_sys(xdbl a) { return Xdbl_ToDouble(a); }
xdbl Xdbl_FromDouble_imctk_abc_sys(double a) { return Xdbl_FromDouble(a); }
xdbl Xdbl_Add_imctk_abc_sys(xdbl a, xdbl b) { return Xdbl_Add(a, b); }
xdbl Xdbl_Mul_imctk_abc_sys(xdbl a, xdbl b) { return Xdbl_Mul(a, b); }
xdbl Xdbl_Div_imctk_abc_sys(xdbl a, unsigned int Deg2) { return Xdbl_Div(a, Deg2); }
void Xdbl_Test_imctk_abc_sys(void) { Xdbl_Test(); }
clause * clause_read_imctk_abc_sys(sat_solver *s, cla h) { return clause_read(s, h); }
int sat_solver_var_value_imctk_abc_sys(sat_solver *s, int v) { return sat_solver_var_value(s, v); }
int sat_solver_var_literal_imctk_abc_sys(sat_solver *s, int v) { return sat_solver_var_literal(s, v); }
void sat_solver_flip_print_clause_imctk_abc_sys(sat_solver *s) { sat_solver_flip_print_clause(s); }
void sat_solver_act_var_clear_imctk_abc_sys(sat_solver *s) { sat_solver_act_var_clear(s); }
void sat_solver_compress_imctk_abc_sys(sat_solver *s) { sat_solver_compress(s); }
void sat_solver_delete_p_imctk_abc_sys(sat_solver **ps) { sat_solver_delete_p(ps); }
void sat_solver_clean_polarity_imctk_abc_sys(sat_solver *s, int *pVars, int nVars) { sat_solver_clean_polarity(s, pVars, nVars); }
void sat_solver_set_polarity_imctk_abc_sys(sat_solver *s, int *pVars, int nVars) { sat_solver_set_polarity(s, pVars, nVars); }
void sat_solver_set_literal_polarity_imctk_abc_sys(sat_solver *s, int *pLits, int nLits) { sat_solver_set_literal_polarity(s, pLits, nLits); }
int sat_solver_final_imctk_abc_sys(sat_solver *s, int **ppArray) { return sat_solver_final(s, ppArray); }
void sat_solver_randomize_imctk_abc_sys(sat_solver *pSat, int iVar, int nVars) { sat_solver_randomize(pSat, iVar, nVars); }
abctime sat_solver_set_runtime_limit_imctk_abc_sys(sat_solver *s, abctime Limit) { return sat_solver_set_runtime_limit(s, Limit); }
int sat_solver_set_random_imctk_abc_sys(sat_solver *s, int fNotUseRandom) { return sat_solver_set_random(s, fNotUseRandom); }
void sat_solver_bookmark_imctk_abc_sys(sat_solver *s) { sat_solver_bookmark(s); }
void sat_solver_set_pivot_variables_imctk_abc_sys(sat_solver *s, int *pPivots, int nPivots) { sat_solver_set_pivot_variables(s, pPivots, nPivots); }
int sat_solver_count_usedvars_imctk_abc_sys(sat_solver *s) { return sat_solver_count_usedvars(s); }
void sat_solver_set_runid_imctk_abc_sys(sat_solver *s, int id) { sat_solver_set_runid(s, id); }
void sat_solver_set_stop_func_imctk_abc_sys(sat_solver *s, int (*fnct) (int)) { sat_solver_set_stop_func(s, fnct); }
int sat_solver_add_const_imctk_abc_sys(sat_solver *pSat, int iVar, int fCompl) { return sat_solver_add_const(pSat, iVar, fCompl); }
int sat_solver_add_buffer_imctk_abc_sys(sat_solver *pSat, int iVarA, int iVarB, int fCompl) { return sat_solver_add_buffer(pSat, iVarA, iVarB, fCompl); }
int sat_solver_add_buffer_enable_imctk_abc_sys(sat_solver *pSat, int iVarA, int iVarB, int iVarEn, int fCompl) { return sat_solver_add_buffer_enable(pSat, iVarA, iVarB, iVarEn, fCompl); }
int sat_solver_add_and_imctk_abc_sys(sat_solver *pSat, int iVar, int iVar0, int iVar1, int fCompl0, int fCompl1, int fCompl) { return sat_solver_add_and(pSat, iVar, iVar0, iVar1, fCompl0, fCompl1, fCompl); }
int sat_solver_add_xor_imctk_abc_sys(sat_solver *pSat, int iVarA, int iVarB, int iVarC, int fCompl) { return sat_solver_add_xor(pSat, iVarA, iVarB, iVarC, fCompl); }
int sat_solver_add_mux_imctk_abc_sys(sat_solver *pSat, int iVarZ, int iVarC, int iVarT, int iVarE, int iComplC, int iComplT, int iComplE, int iComplZ) { return sat_solver_add_mux(pSat, iVarZ, iVarC, iVarT, iVarE, iComplC, iComplT, iComplE, iComplZ); }
int sat_solver_add_mux41_imctk_abc_sys(sat_solver *pSat, int iVarZ, int iVarC0, int iVarC1, int iVarD0, int iVarD1, int iVarD2, int iVarD3) { return sat_solver_add_mux41(pSat, iVarZ, iVarC0, iVarC1, iVarD0, iVarD1, iVarD2, iVarD3); }
int sat_solver_add_xor_and_imctk_abc_sys(sat_solver *pSat, int iVarF, int iVarA, int iVarB, int iVarC) { return sat_solver_add_xor_and(pSat, iVarF, iVarA, iVarB, iVarC); }
int sat_solver_add_constraint_imctk_abc_sys(sat_solver *pSat, int iVar, int iVar2, int fCompl) { return sat_solver_add_constraint(pSat, iVar, iVar2, fCompl); }
int sat_solver_add_half_sorter_imctk_abc_sys(sat_solver *pSat, int iVarA, int iVarB, int iVar0, int iVar1) { return sat_solver_add_half_sorter(pSat, iVarA, iVarB, iVar0, iVar1); }
sat_solver * Pdr_ManSolver_imctk_abc_sys(Pdr_Man_t *p, int k) { return Pdr_ManSolver(p, k); }
abctime Pdr_ManTimeLimit_imctk_abc_sys(Pdr_Man_t *p) { return Pdr_ManTimeLimit(p); }
