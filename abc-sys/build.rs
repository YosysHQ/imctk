#![allow(missing_docs)] // build script
use std::path::PathBuf;

static SRC_FILES: &[&str] = &[
    "abc/src/aig/aig/aigCanon.c",
    "abc/src/aig/aig/aigCheck.c",
    "abc/src/aig/aig/aigCuts.c",
    "abc/src/aig/aig/aigDfs.c",
    "abc/src/aig/aig/aigDup.c",
    "abc/src/aig/aig/aigFanout.c",
    "abc/src/aig/aig/aigFrames.c",
    "abc/src/aig/aig/aigInter.c",
    "abc/src/aig/aig/aigJust.c",
    "abc/src/aig/aig/aigMan.c",
    "abc/src/aig/aig/aigMem.c",
    "abc/src/aig/aig/aigMffc.c",
    "abc/src/aig/aig/aigObj.c",
    "abc/src/aig/aig/aigOper.c",
    "abc/src/aig/aig/aigOrder.c",
    "abc/src/aig/aig/aigPack.c",
    "abc/src/aig/aig/aigPart.c",
    "abc/src/aig/aig/aigPartReg.c",
    "abc/src/aig/aig/aigPartSat.c",
    "abc/src/aig/aig/aigRepr.c",
    "abc/src/aig/aig/aigRet.c",
    "abc/src/aig/aig/aigRetF.c",
    "abc/src/aig/aig/aigScl.c",
    "abc/src/aig/aig/aigShow.c",
    "abc/src/aig/aig/aigSplit.c",
    "abc/src/aig/aig/aigTable.c",
    "abc/src/aig/aig/aigTiming.c",
    "abc/src/aig/aig/aigTruth.c",
    "abc/src/aig/aig/aigTsim.c",
    "abc/src/aig/aig/aigUtil.c",
    "abc/src/aig/aig/aigWin.c",
    "abc/src/aig/gia/giaAgi.c",
    "abc/src/aig/gia/giaAig.c",
    "abc/src/aig/gia/giaAiger.c",
    "abc/src/aig/gia/giaAigerExt.c",
    "abc/src/aig/gia/giaBalAig.c",
    "abc/src/aig/gia/giaBalLut.c",
    "abc/src/aig/gia/giaBalMap.c",
    "abc/src/aig/gia/giaBidec.c",
    "abc/src/aig/gia/giaBound.c",
    "abc/src/aig/gia/giaCCof.c",
    "abc/src/aig/gia/giaCex.c",
    "abc/src/aig/gia/giaClp.c",
    "abc/src/aig/gia/giaCof.c",
    "abc/src/aig/gia/giaCone.c",
    "abc/src/aig/gia/giaCSat2.c",
    "abc/src/aig/gia/giaCSat3.c",
    "abc/src/aig/gia/giaCSat.c",
    "abc/src/aig/gia/giaCSatOld.c",
    "abc/src/aig/gia/giaCSatP.c",
    "abc/src/aig/gia/giaCTas.c",
    "abc/src/aig/gia/giaCut.c",
    "abc/src/aig/gia/giaDecs.c",
    "abc/src/aig/gia/giaDeep.c",
    "abc/src/aig/gia/giaDfs.c",
    "abc/src/aig/gia/giaDup.c",
    "abc/src/aig/gia/giaEdge.c",
    "abc/src/aig/gia/giaEmbed.c",
    "abc/src/aig/gia/giaEnable.c",
    "abc/src/aig/gia/giaEquiv.c",
    "abc/src/aig/gia/giaEra2.c",
    "abc/src/aig/gia/giaEra.c",
    "abc/src/aig/gia/giaEsop.c",
    "abc/src/aig/gia/giaExist.c",
    "abc/src/aig/gia/giaFalse.c",
    "abc/src/aig/gia/giaFanout.c",
    "abc/src/aig/gia/giaForce.c",
    "abc/src/aig/gia/giaFrames.c",
    "abc/src/aig/gia/giaFront.c",
    "abc/src/aig/gia/giaFx.c",
    "abc/src/aig/gia/giaGen.c",
    "abc/src/aig/gia/giaGig.c",
    "abc/src/aig/gia/giaGlitch.c",
    "abc/src/aig/gia/giaHash.c",
    "abc/src/aig/gia/giaIf.c",
    "abc/src/aig/gia/giaIff.c",
    "abc/src/aig/gia/giaIiff.c",
    "abc/src/aig/gia/giaIso2.c",
    "abc/src/aig/gia/giaIso3.c",
    "abc/src/aig/gia/giaIso.c",
    "abc/src/aig/gia/giaJf.c",
    "abc/src/aig/gia/giaKf.c",
    "abc/src/aig/gia/giaLf.c",
    "abc/src/aig/gia/giaMan.c",
    "abc/src/aig/gia/giaMem.c",
    "abc/src/aig/gia/giaMf.c",
    "abc/src/aig/gia/giaMfs.c",
    "abc/src/aig/gia/giaMini.c",
    "abc/src/aig/gia/giaMinLut2.c",
    "abc/src/aig/gia/giaMinLut.c",
    "abc/src/aig/gia/giaMuxes.c",
    "abc/src/aig/gia/giaNf.c",
    "abc/src/aig/gia/giaOf.c",
    "abc/src/aig/gia/giaPack.c",
    "abc/src/aig/gia/giaPat2.c",
    "abc/src/aig/gia/giaPat.c",
    "abc/src/aig/gia/giaPf.c",
    "abc/src/aig/gia/giaQbf.c",
    "abc/src/aig/gia/giaReshape1.c",
    "abc/src/aig/gia/giaReshape2.c",
    "abc/src/aig/gia/giaResub2.c",
    "abc/src/aig/gia/giaResub3.c",
    "abc/src/aig/gia/giaResub6.c",
    "abc/src/aig/gia/giaResub.c",
    "abc/src/aig/gia/giaRetime.c",
    "abc/src/aig/gia/giaRex.c",
    "abc/src/aig/gia/giaSat3.c",
    "abc/src/aig/gia/giaSatEdge.c",
    "abc/src/aig/gia/giaSatLE.c",
    "abc/src/aig/gia/giaSatLut.c",
    "abc/src/aig/gia/giaSatMap.c",
    "abc/src/aig/gia/giaSatoko.c",
    "abc/src/aig/gia/giaSatSyn.c",
    "abc/src/aig/gia/giaScl.c",
    "abc/src/aig/gia/giaScript.c",
    "abc/src/aig/gia/giaShow.c",
    "abc/src/aig/gia/giaShrink6.c",
    "abc/src/aig/gia/giaShrink7.c",
    "abc/src/aig/gia/giaShrink.c",
    "abc/src/aig/gia/giaSif.c",
    "abc/src/aig/gia/giaSim2.c",
    "abc/src/aig/gia/giaSimBase.c",
    "abc/src/aig/gia/giaSim.c",
    "abc/src/aig/gia/giaSort.c",
    "abc/src/aig/gia/giaSpeedup.c",
    "abc/src/aig/gia/giaSplit.c",
    "abc/src/aig/gia/giaStg.c",
    "abc/src/aig/gia/giaStoch.c",
    "abc/src/aig/gia/giaStr.c",
    "abc/src/aig/gia/giaSupMin.c",
    "abc/src/aig/gia/giaSupp.c",
    "abc/src/aig/gia/giaSupps.c",
    "abc/src/aig/gia/giaSweep.c",
    "abc/src/aig/gia/giaSweeper.c",
    "abc/src/aig/gia/giaSwitch.c",
    "abc/src/aig/gia/giaTim.c",
    "abc/src/aig/gia/giaTis.c",
    "abc/src/aig/gia/giaTransduction.cpp",
    "abc/src/aig/gia/giaTranStoch.c",
    "abc/src/aig/gia/giaTruth.c",
    "abc/src/aig/gia/giaTsim.c",
    "abc/src/aig/gia/giaTtopt.cpp",
    "abc/src/aig/gia/giaUnate.c",
    "abc/src/aig/gia/giaUtil.c",
    "abc/src/aig/hop/hopBalance.c",
    "abc/src/aig/hop/hopCheck.c",
    "abc/src/aig/hop/hopDfs.c",
    "abc/src/aig/hop/hopMan.c",
    "abc/src/aig/hop/hopMem.c",
    "abc/src/aig/hop/hopObj.c",
    "abc/src/aig/hop/hopOper.c",
    "abc/src/aig/hop/hopTable.c",
    "abc/src/aig/hop/hopTruth.c",
    "abc/src/aig/hop/hopUtil.c",
    "abc/src/aig/ioa/ioaReadAig.c",
    "abc/src/aig/ioa/ioaUtil.c",
    "abc/src/aig/ioa/ioaWriteAig.c",
    "abc/src/aig/ivy/ivyBalance.c",
    "abc/src/aig/ivy/ivyCanon.c",
    "abc/src/aig/ivy/ivyCheck.c",
    "abc/src/aig/ivy/ivyCut.c",
    "abc/src/aig/ivy/ivyCutTrav.c",
    "abc/src/aig/ivy/ivyDfs.c",
    "abc/src/aig/ivy/ivyDsd.c",
    "abc/src/aig/ivy/ivyFanout.c",
    "abc/src/aig/ivy/ivyFastMap.c",
    "abc/src/aig/ivy/ivyFraig.c",
    "abc/src/aig/ivy/ivyHaig.c",
    "abc/src/aig/ivy/ivyMan.c",
    "abc/src/aig/ivy/ivyMem.c",
    "abc/src/aig/ivy/ivyMulti.c",
    "abc/src/aig/ivy/ivyObj.c",
    "abc/src/aig/ivy/ivyOper.c",
    "abc/src/aig/ivy/ivyResyn.c",
    "abc/src/aig/ivy/ivyRwr.c",
    "abc/src/aig/ivy/ivySeq.c",
    "abc/src/aig/ivy/ivyShow.c",
    "abc/src/aig/ivy/ivyTable.c",
    "abc/src/aig/ivy/ivyUtil.c",
    "abc/src/aig/saig/saigCone.c",
    "abc/src/aig/saig/saigConstr2.c",
    "abc/src/aig/saig/saigConstr.c",
    "abc/src/aig/saig/saigDual.c",
    "abc/src/aig/saig/saigDup.c",
    "abc/src/aig/saig/saigInd.c",
    "abc/src/aig/saig/saigIoa.c",
    "abc/src/aig/saig/saigIso.c",
    "abc/src/aig/saig/saigIsoFast.c",
    "abc/src/aig/saig/saigIsoSlow.c",
    "abc/src/aig/saig/saigMiter.c",
    "abc/src/aig/saig/saigOutDec.c",
    "abc/src/aig/saig/saigPhase.c",
    "abc/src/aig/saig/saigRetFwd.c",
    "abc/src/aig/saig/saigRetMin.c",
    "abc/src/aig/saig/saigRetStep.c",
    "abc/src/aig/saig/saigScl.c",
    "abc/src/aig/saig/saigSimFast.c",
    "abc/src/aig/saig/saigSimMv.c",
    "abc/src/aig/saig/saigSimSeq.c",
    "abc/src/aig/saig/saigStrSim.c",
    "abc/src/aig/saig/saigSwitch.c",
    "abc/src/aig/saig/saigSynch.c",
    "abc/src/aig/saig/saigTempor.c",
    "abc/src/aig/saig/saigTrans.c",
    "abc/src/aig/saig/saigWnd.c",
    "abc/src/base/abc/abcAig.c",
    "abc/src/base/abc/abcBarBuf.c",
    "abc/src/base/abc/abcBlifMv.c",
    "abc/src/base/abc/abcCheck.c",
    "abc/src/base/abc/abcDfs.c",
    "abc/src/base/abc/abcFanio.c",
    "abc/src/base/abc/abcFanOrder.c",
    "abc/src/base/abc/abcFunc.c",
    "abc/src/base/abc/abcHie.c",
    "abc/src/base/abc/abcHieCec.c",
    "abc/src/base/abc/abcHieGia.c",
    "abc/src/base/abc/abcHieNew.c",
    "abc/src/base/abc/abcLatch.c",
    "abc/src/base/abc/abcLib.c",
    "abc/src/base/abc/abcMinBase.c",
    "abc/src/base/abc/abcNames.c",
    "abc/src/base/abc/abcNetlist.c",
    "abc/src/base/abc/abcNtk.c",
    "abc/src/base/abc/abcObj.c",
    "abc/src/base/abc/abcRefs.c",
    "abc/src/base/abc/abcShow.c",
    "abc/src/base/abc/abcSop.c",
    "abc/src/base/abc/abcUtil.c",
    "abc/src/base/abci/abcAttach.c",
    "abc/src/base/abci/abcAuto.c",
    "abc/src/base/abci/abcBalance.c",
    "abc/src/base/abci/abcBidec.c",
    "abc/src/base/abci/abcBm.c",
    "abc/src/base/abci/abcBmc.c",
    "abc/src/base/abci/abc.c",
    "abc/src/base/abci/abcCas.c",
    "abc/src/base/abci/abcCascade.c",
    "abc/src/base/abci/abcCollapse.c",
    "abc/src/base/abci/abcCut.c",
    "abc/src/base/abci/abcDar.c",
    "abc/src/base/abci/abcDebug.c",
    "abc/src/base/abci/abcDec.c",
    "abc/src/base/abci/abcDetect.c",
    "abc/src/base/abci/abcDress2.c",
    "abc/src/base/abci/abcDress3.c",
    "abc/src/base/abci/abcDress.c",
    "abc/src/base/abci/abcDsd.c",
    "abc/src/base/abci/abcEco.c",
    "abc/src/base/abci/abcExact.c",
    "abc/src/base/abci/abcExtract.c",
    "abc/src/base/abci/abcFraig.c",
    "abc/src/base/abci/abcFx.c",
    "abc/src/base/abci/abcFxu.c",
    "abc/src/base/abci/abcGen.c",
    "abc/src/base/abci/abcHaig.c",
    "abc/src/base/abci/abcIf.c",
    "abc/src/base/abci/abcIfif.c",
    "abc/src/base/abci/abcIfMux.c",
    "abc/src/base/abci/abcIvy.c",
    "abc/src/base/abci/abcLog.c",
    "abc/src/base/abci/abcLut.c",
    "abc/src/base/abci/abcLutmin.c",
    "abc/src/base/abci/abcMap.c",
    "abc/src/base/abci/abcMerge.c",
    "abc/src/base/abci/abcMfs.c",
    "abc/src/base/abci/abcMini.c",
    "abc/src/base/abci/abcMiter.c",
    "abc/src/base/abci/abcMulti.c",
    "abc/src/base/abci/abcNpn.c",
    "abc/src/base/abci/abcNpnSave.c",
    "abc/src/base/abci/abcNtbdd.c",
    "abc/src/base/abci/abcOdc.c",
    "abc/src/base/abci/abcOrchestration.c",
    "abc/src/base/abci/abcOrder.c",
    "abc/src/base/abci/abcPart.c",
    "abc/src/base/abci/abcPrint.c",
    "abc/src/base/abci/abcProve.c",
    "abc/src/base/abci/abcQbf.c",
    "abc/src/base/abci/abcQuant.c",
    "abc/src/base/abci/abcReach.c",
    "abc/src/base/abci/abcRec3.c",
    "abc/src/base/abci/abcReconv.c",
    "abc/src/base/abci/abcRefactor.c",
    "abc/src/base/abci/abcRenode.c",
    "abc/src/base/abci/abcReorder.c",
    "abc/src/base/abci/abcRestruct.c",
    "abc/src/base/abci/abcResub.c",
    "abc/src/base/abci/abcRewrite.c",
    "abc/src/base/abci/abcRpo.c",
    "abc/src/base/abci/abcRr.c",
    "abc/src/base/abci/abcRunGen.c",
    "abc/src/base/abci/abcSat.c",
    "abc/src/base/abci/abcSaucy.c",
    "abc/src/base/abci/abcScorr.c",
    "abc/src/base/abci/abcSense.c",
    "abc/src/base/abci/abcSpeedup.c",
    "abc/src/base/abci/abcStrash.c",
    "abc/src/base/abci/abcSweep.c",
    "abc/src/base/abci/abcSymm.c",
    "abc/src/base/abci/abcTim.c",
    "abc/src/base/abci/abcTiming.c",
    "abc/src/base/abci/abcUnate.c",
    "abc/src/base/abci/abcUnreach.c",
    "abc/src/base/abci/abcVerify.c",
    "abc/src/base/abci/abcXsim.c",
    "abc/src/base/acb/acbAbc.c",
    "abc/src/base/acb/acbAig.c",
    "abc/src/base/acb/acbCom.c",
    "abc/src/base/acb/acbFunc.c",
    "abc/src/base/acb/acbMfs.c",
    "abc/src/base/acb/acbPush.c",
    "abc/src/base/acb/acbSets.c",
    "abc/src/base/acb/acbTest.c",
    "abc/src/base/acb/acbUtil.c",
    "abc/src/base/bac/bacBac.c",
    "abc/src/base/bac/bacBlast.c",
    "abc/src/base/bac/bacCom.c",
    "abc/src/base/bac/bacLib.c",
    "abc/src/base/bac/bacNtk.c",
    "abc/src/base/bac/bacPrsBuild.c",
    "abc/src/base/bac/bacPrsTrans.c",
    "abc/src/base/bac/bacPtrAbc.c",
    "abc/src/base/bac/bacPtr.c",
    "abc/src/base/bac/bacReadBlif.c",
    "abc/src/base/bac/bacReadSmt.c",
    "abc/src/base/bac/bacReadVer.c",
    "abc/src/base/bac/bacWriteBlif.c",
    "abc/src/base/bac/bacWriteSmt.c",
    "abc/src/base/bac/bacWriteVer.c",
    "abc/src/base/cba/cbaBlast.c",
    "abc/src/base/cba/cbaCba.c",
    "abc/src/base/cba/cbaCom.c",
    "abc/src/base/cba/cbaNtk.c",
    "abc/src/base/cba/cbaReadBlif.c",
    "abc/src/base/cba/cbaReadVer.c",
    "abc/src/base/cba/cbaWriteBlif.c",
    "abc/src/base/cba/cbaWriteVer.c",
    "abc/src/base/cmd/cmdAlias.c",
    "abc/src/base/cmd/cmdApi.c",
    "abc/src/base/cmd/cmdAuto.c",
    "abc/src/base/cmd/cmd.c",
    "abc/src/base/cmd/cmdFlag.c",
    "abc/src/base/cmd/cmdHist.c",
    "abc/src/base/cmd/cmdLoad.c",
    "abc/src/base/cmd/cmdPlugin.c",
    "abc/src/base/cmd/cmdStarter.c",
    "abc/src/base/cmd/cmdUtils.c",
    "abc/src/base/exor/exorBits.c",
    "abc/src/base/exor/exor.c",
    "abc/src/base/exor/exorCubes.c",
    "abc/src/base/exor/exorLink.c",
    "abc/src/base/exor/exorList.c",
    "abc/src/base/exor/exorUtil.c",
    "abc/src/base/io/io.c",
    "abc/src/base/io/ioJson.c",
    "abc/src/base/io/ioReadAiger.c",
    "abc/src/base/io/ioReadBaf.c",
    "abc/src/base/io/ioReadBblif.c",
    "abc/src/base/io/ioReadBench.c",
    "abc/src/base/io/ioReadBlifAig.c",
    "abc/src/base/io/ioReadBlif.c",
    "abc/src/base/io/ioReadBlifMv.c",
    "abc/src/base/io/ioReadDsd.c",
    "abc/src/base/io/ioReadEdif.c",
    "abc/src/base/io/ioReadEqn.c",
    "abc/src/base/io/ioReadPla.c",
    "abc/src/base/io/ioReadPlaMo.c",
    "abc/src/base/io/ioReadVerilog.c",
    "abc/src/base/io/ioUtil.c",
    "abc/src/base/io/ioWriteAiger.c",
    "abc/src/base/io/ioWriteBaf.c",
    "abc/src/base/io/ioWriteBblif.c",
    "abc/src/base/io/ioWriteBench.c",
    "abc/src/base/io/ioWriteBlif.c",
    "abc/src/base/io/ioWriteBlifMv.c",
    "abc/src/base/io/ioWriteBook.c",
    "abc/src/base/io/ioWriteCnf.c",
    "abc/src/base/io/ioWriteDot.c",
    "abc/src/base/io/ioWriteEdgelist.c",
    "abc/src/base/io/ioWriteEqn.c",
    "abc/src/base/io/ioWriteGml.c",
    "abc/src/base/io/ioWriteList.c",
    "abc/src/base/io/ioWritePla.c",
    "abc/src/base/io/ioWriteSmv.c",
    "abc/src/base/io/ioWriteVerilog.c",
    "abc/src/base/main/libSupport.c",
    "abc/src/base/main/main.c",
    "abc/src/base/main/mainFrame.c",
    "abc/src/base/main/mainInit.c",
    "abc/src/base/main/mainLib.c",
    "abc/src/base/main/mainReal.c",
    "abc/src/base/main/mainUtils.c",
    "abc/src/base/pla/plaCom.c",
    "abc/src/base/pla/plaHash.c",
    "abc/src/base/pla/plaMan.c",
    "abc/src/base/pla/plaMerge.c",
    "abc/src/base/pla/plaRead.c",
    "abc/src/base/pla/plaSimple.c",
    "abc/src/base/pla/plaWrite.c",
    "abc/src/base/test/test.c",
    "abc/src/base/ver/verCore.c",
    "abc/src/base/ver/verFormula.c",
    "abc/src/base/ver/verParse.c",
    "abc/src/base/ver/verStream.c",
    "abc/src/base/wlc/wlcAbc.c",
    "abc/src/base/wlc/wlcAbs2.c",
    "abc/src/base/wlc/wlcAbs.c",
    "abc/src/base/wlc/wlcBlast.c",
    "abc/src/base/wlc/wlcCom.c",
    "abc/src/base/wlc/wlcGraft.c",
    "abc/src/base/wlc/wlcJson.c",
    "abc/src/base/wlc/wlcMem.c",
    "abc/src/base/wlc/wlcNdr.c",
    "abc/src/base/wlc/wlcNtk.c",
    "abc/src/base/wlc/wlcPth.c",
    "abc/src/base/wlc/wlcReadSmt.c",
    "abc/src/base/wlc/wlcReadVer.c",
    "abc/src/base/wlc/wlcShow.c",
    "abc/src/base/wlc/wlcSim.c",
    "abc/src/base/wlc/wlcStdin.c",
    "abc/src/base/wlc/wlcUif.c",
    "abc/src/base/wlc/wlcWin.c",
    "abc/src/base/wlc/wlcWriteVer.c",
    "abc/src/base/wln/wlnBlast.c",
    "abc/src/base/wln/wln.c",
    "abc/src/base/wln/wlnCom.c",
    "abc/src/base/wln/wlnGuide.c",
    "abc/src/base/wln/wlnMem.c",
    "abc/src/base/wln/wlnNdr.c",
    "abc/src/base/wln/wlnNtk.c",
    "abc/src/base/wln/wlnObj.c",
    "abc/src/base/wln/wlnRead.c",
    "abc/src/base/wln/wlnRetime.c",
    "abc/src/base/wln/wlnRtl.c",
    "abc/src/base/wln/wlnWlc.c",
    "abc/src/base/wln/wlnWriteVer.c",
    "abc/src/bool/bdc/bdcCore.c",
    "abc/src/bool/bdc/bdcDec.c",
    "abc/src/bool/bdc/bdcSpfd.c",
    "abc/src/bool/bdc/bdcTable.c",
    "abc/src/bool/dec/decAbc.c",
    "abc/src/bool/dec/decFactor.c",
    "abc/src/bool/dec/decMan.c",
    "abc/src/bool/dec/decPrint.c",
    "abc/src/bool/dec/decUtil.c",
    "abc/src/bool/kit/cloud.c",
    "abc/src/bool/kit/kitAig.c",
    "abc/src/bool/kit/kitBdd.c",
    "abc/src/bool/kit/kitCloud.c",
    "abc/src/bool/kit/kitDsd.c",
    "abc/src/bool/kit/kitFactor.c",
    "abc/src/bool/kit/kitGraph.c",
    "abc/src/bool/kit/kitHop.c",
    "abc/src/bool/kit/kitIsop.c",
    "abc/src/bool/kit/kitPla.c",
    "abc/src/bool/kit/kitSop.c",
    "abc/src/bool/kit/kitTruth.c",
    "abc/src/bool/lucky/lucky.c",
    "abc/src/bool/lucky/luckyFast16.c",
    "abc/src/bool/lucky/luckyFast6.c",
    "abc/src/bool/lucky/luckyRead.c",
    "abc/src/bool/lucky/luckySimple.c",
    "abc/src/bool/lucky/luckySwap.c",
    "abc/src/bool/lucky/luckySwapIJ.c",
    "abc/src/bool/rpo/rpo.c",
    "abc/src/bool/rsb/rsbDec6.c",
    "abc/src/bool/rsb/rsbMan.c",
    "abc/src/map/amap/amapCore.c",
    "abc/src/map/amap/amapGraph.c",
    "abc/src/map/amap/amapLib.c",
    "abc/src/map/amap/amapLiberty.c",
    "abc/src/map/amap/amapMan.c",
    "abc/src/map/amap/amapMatch.c",
    "abc/src/map/amap/amapMerge.c",
    "abc/src/map/amap/amapOutput.c",
    "abc/src/map/amap/amapParse.c",
    "abc/src/map/amap/amapPerm.c",
    "abc/src/map/amap/amapRead.c",
    "abc/src/map/amap/amapRule.c",
    "abc/src/map/amap/amapUniq.c",
    "abc/src/map/cov/covBuild.c",
    "abc/src/map/cov/covCore.c",
    "abc/src/map/cov/covMan.c",
    "abc/src/map/cov/covMinEsop.c",
    "abc/src/map/cov/covMinMan.c",
    "abc/src/map/cov/covMinSop.c",
    "abc/src/map/cov/covMinUtil.c",
    "abc/src/map/if/acd/ac_wrapper.cpp",
    "abc/src/map/if/ifCache.c",
    "abc/src/map/if/ifCom.c",
    "abc/src/map/if/ifCore.c",
    "abc/src/map/if/ifCut.c",
    "abc/src/map/if/ifData2.c",
    "abc/src/map/if/ifDec07.c",
    "abc/src/map/if/ifDec08.c",
    "abc/src/map/if/ifDec10.c",
    "abc/src/map/if/ifDec16.c",
    "abc/src/map/if/ifDec66.c",
    "abc/src/map/if/ifDec75.c",
    "abc/src/map/if/ifDelay.c",
    "abc/src/map/if/ifDsd.c",
    "abc/src/map/if/ifLibBox.c",
    "abc/src/map/if/ifLibLut.c",
    "abc/src/map/if/ifMan.c",
    "abc/src/map/if/ifMap.c",
    "abc/src/map/if/ifMatch2.c",
    "abc/src/map/if/ifReduce.c",
    "abc/src/map/if/ifSat.c",
    "abc/src/map/if/ifSelect.c",
    "abc/src/map/if/ifSeq.c",
    "abc/src/map/if/ifTest.c",
    "abc/src/map/if/ifTime.c",
    "abc/src/map/if/ifTruth.c",
    "abc/src/map/if/ifTune.c",
    "abc/src/map/if/ifUtil.c",
    "abc/src/map/mapper/mapper.c",
    "abc/src/map/mapper/mapperCanon.c",
    "abc/src/map/mapper/mapperCore.c",
    "abc/src/map/mapper/mapperCreate.c",
    "abc/src/map/mapper/mapperCut.c",
    "abc/src/map/mapper/mapperCutUtils.c",
    "abc/src/map/mapper/mapperLib.c",
    "abc/src/map/mapper/mapperMatch.c",
    "abc/src/map/mapper/mapperRefs.c",
    "abc/src/map/mapper/mapperSuper.c",
    "abc/src/map/mapper/mapperSwitch.c",
    "abc/src/map/mapper/mapperTable.c",
    "abc/src/map/mapper/mapperTime.c",
    "abc/src/map/mapper/mapperTree.c",
    "abc/src/map/mapper/mapperTruth.c",
    "abc/src/map/mapper/mapperUtils.c",
    "abc/src/map/mapper/mapperVec.c",
    "abc/src/map/mio/mioApi.c",
    "abc/src/map/mio/mio.c",
    "abc/src/map/mio/mioFunc.c",
    "abc/src/map/mio/mioParse.c",
    "abc/src/map/mio/mioRead.c",
    "abc/src/map/mio/mioSop.c",
    "abc/src/map/mio/mioUtils.c",
    "abc/src/map/mpm/mpmAbc.c",
    "abc/src/map/mpm/mpmCore.c",
    "abc/src/map/mpm/mpmDsd.c",
    "abc/src/map/mpm/mpmGates.c",
    "abc/src/map/mpm/mpmLib.c",
    "abc/src/map/mpm/mpmMan.c",
    "abc/src/map/mpm/mpmMap.c",
    "abc/src/map/mpm/mpmMig.c",
    "abc/src/map/mpm/mpmPre.c",
    "abc/src/map/mpm/mpmTruth.c",
    "abc/src/map/mpm/mpmUtil.c",
    "abc/src/map/scl/sclBuffer.c",
    "abc/src/map/scl/sclBufSize.c",
    "abc/src/map/scl/scl.c",
    "abc/src/map/scl/sclDnsize.c",
    "abc/src/map/scl/sclLiberty.c",
    "abc/src/map/scl/sclLibScl.c",
    "abc/src/map/scl/sclLibUtil.c",
    "abc/src/map/scl/sclLoad.c",
    "abc/src/map/scl/sclSize.c",
    "abc/src/map/scl/sclUpsize.c",
    "abc/src/map/scl/sclUtil.c",
    "abc/src/map/super/superAnd.c",
    "abc/src/map/super/super.c",
    "abc/src/map/super/superGate.c",
    "abc/src/misc/bar/bar.c",
    "abc/src/misc/bbl/bblif.c",
    "abc/src/misc/bzlib/blocksort.c",
    "abc/src/misc/bzlib/bzlib.c",
    "abc/src/misc/bzlib/compress.c",
    "abc/src/misc/bzlib/crctable.c",
    "abc/src/misc/bzlib/decompress.c",
    "abc/src/misc/bzlib/huffman.c",
    "abc/src/misc/bzlib/randtable.c",
    "abc/src/misc/extra/extraUtilBitMatrix.c",
    "abc/src/misc/extra/extraUtilCanon.c",
    "abc/src/misc/extra/extraUtilCfs.c",
    "abc/src/misc/extra/extraUtilCube.c",
    "abc/src/misc/extra/extraUtilDsd.c",
    "abc/src/misc/extra/extraUtilEnum.c",
    "abc/src/misc/extra/extraUtilFile.c",
    "abc/src/misc/extra/extraUtilGen.c",
    "abc/src/misc/extra/extraUtilMacc.c",
    "abc/src/misc/extra/extraUtilMaj.c",
    "abc/src/misc/extra/extraUtilMemory.c",
    "abc/src/misc/extra/extraUtilMisc.c",
    "abc/src/misc/extra/extraUtilMult.c",
    "abc/src/misc/extra/extraUtilPath.c",
    "abc/src/misc/extra/extraUtilPerm.c",
    "abc/src/misc/extra/extraUtilProgress.c",
    "abc/src/misc/extra/extraUtilReader.c",
    "abc/src/misc/extra/extraUtilSupp.c",
    "abc/src/misc/extra/extraUtilTruth.c",
    "abc/src/misc/extra/extraUtilUtil.c",
    "abc/src/misc/mem/mem.c",
    "abc/src/misc/mvc/mvcApi.c",
    "abc/src/misc/mvc/mvcCompare.c",
    "abc/src/misc/mvc/mvcContain.c",
    "abc/src/misc/mvc/mvcCover.c",
    "abc/src/misc/mvc/mvcCube.c",
    "abc/src/misc/mvc/mvcDivide.c",
    "abc/src/misc/mvc/mvcDivisor.c",
    "abc/src/misc/mvc/mvcList.c",
    "abc/src/misc/mvc/mvcLits.c",
    "abc/src/misc/mvc/mvcMan.c",
    "abc/src/misc/mvc/mvcOpAlg.c",
    "abc/src/misc/mvc/mvcOpBool.c",
    "abc/src/misc/mvc/mvcPrint.c",
    "abc/src/misc/mvc/mvcSort.c",
    "abc/src/misc/mvc/mvcUtils.c",
    "abc/src/misc/nm/nmApi.c",
    "abc/src/misc/nm/nmTable.c",
    "abc/src/misc/parse/parseEqn.c",
    "abc/src/misc/parse/parseStack.c",
    "abc/src/misc/st/st.c",
    "abc/src/misc/st/stmm.c",
    "abc/src/misc/tim/timBox.c",
    "abc/src/misc/tim/timDump.c",
    "abc/src/misc/tim/timMan.c",
    "abc/src/misc/tim/timTime.c",
    "abc/src/misc/tim/timTrav.c",
    "abc/src/misc/util/utilBridge.c",
    "abc/src/misc/util/utilCex.c",
    "abc/src/misc/util/utilColor.c",
    "abc/src/misc/util/utilFile.c",
    "abc/src/misc/util/utilIsop.c",
    "abc/src/misc/util/utilNam.c",
    "abc/src/misc/util/utilSignal.c",
    "abc/src/misc/util/utilSort.c",
    "abc/src/misc/zlib/adler32.c",
    "abc/src/misc/zlib/compress_.c",
    "abc/src/misc/zlib/crc32.c",
    "abc/src/misc/zlib/deflate.c",
    "abc/src/misc/zlib/gzclose.c",
    "abc/src/misc/zlib/gzlib.c",
    "abc/src/misc/zlib/gzread.c",
    "abc/src/misc/zlib/gzwrite.c",
    "abc/src/misc/zlib/infback.c",
    "abc/src/misc/zlib/inffast.c",
    "abc/src/misc/zlib/inflate.c",
    "abc/src/misc/zlib/inftrees.c",
    "abc/src/misc/zlib/trees.c",
    "abc/src/misc/zlib/uncompr.c",
    "abc/src/misc/zlib/zutil.c",
    "abc/src/opt/cgt/cgtAig.c",
    "abc/src/opt/cgt/cgtCore.c",
    "abc/src/opt/cgt/cgtDecide.c",
    "abc/src/opt/cgt/cgtMan.c",
    "abc/src/opt/cgt/cgtSat.c",
    "abc/src/opt/csw/cswCore.c",
    "abc/src/opt/csw/cswCut.c",
    "abc/src/opt/csw/cswMan.c",
    "abc/src/opt/csw/cswTable.c",
    "abc/src/opt/cut/cutApi.c",
    "abc/src/opt/cut/cutCut.c",
    "abc/src/opt/cut/cutMan.c",
    "abc/src/opt/cut/cutMerge.c",
    "abc/src/opt/cut/cutNode.c",
    "abc/src/opt/cut/cutOracle.c",
    "abc/src/opt/cut/cutPre22.c",
    "abc/src/opt/cut/cutSeq.c",
    "abc/src/opt/cut/cutTruth.c",
    "abc/src/opt/dar/darBalance.c",
    "abc/src/opt/dar/darCore.c",
    "abc/src/opt/dar/darCut.c",
    "abc/src/opt/dar/darData.c",
    "abc/src/opt/dar/darLib.c",
    "abc/src/opt/dar/darMan.c",
    "abc/src/opt/dar/darPrec.c",
    "abc/src/opt/dar/darRefact.c",
    "abc/src/opt/dar/darScript.c",
    "abc/src/opt/dau/dauCanon.c",
    "abc/src/opt/dau/dauCore.c",
    "abc/src/opt/dau/dauCount.c",
    "abc/src/opt/dau/dauDivs.c",
    "abc/src/opt/dau/dauDsd.c",
    "abc/src/opt/dau/dauEnum.c",
    "abc/src/opt/dau/dauGia.c",
    "abc/src/opt/dau/dauMerge.c",
    "abc/src/opt/dau/dauNonDsd.c",
    "abc/src/opt/dau/dauNpn2.c",
    "abc/src/opt/dau/dauNpn.c",
    "abc/src/opt/dau/dauTree.c",
    "abc/src/opt/dsc/dsc.c",
    "abc/src/opt/fret/fretFlow.c",
    "abc/src/opt/fret/fretInit.c",
    "abc/src/opt/fret/fretMain.c",
    "abc/src/opt/fret/fretTime.c",
    "abc/src/opt/fxch/Fxch.c",
    "abc/src/opt/fxch/FxchDiv.c",
    "abc/src/opt/fxch/FxchMan.c",
    "abc/src/opt/fxch/FxchSCHashTable.c",
    "abc/src/opt/fxu/fxu.c",
    "abc/src/opt/fxu/fxuCreate.c",
    "abc/src/opt/fxu/fxuHeapD.c",
    "abc/src/opt/fxu/fxuHeapS.c",
    "abc/src/opt/fxu/fxuList.c",
    "abc/src/opt/fxu/fxuMatrix.c",
    "abc/src/opt/fxu/fxuPair.c",
    "abc/src/opt/fxu/fxuPrint.c",
    "abc/src/opt/fxu/fxuReduce.c",
    "abc/src/opt/fxu/fxuSelect.c",
    "abc/src/opt/fxu/fxuSingle.c",
    "abc/src/opt/fxu/fxuUpdate.c",
    "abc/src/opt/lpk/lpkAbcDec.c",
    "abc/src/opt/lpk/lpkAbcDsd.c",
    "abc/src/opt/lpk/lpkAbcMux.c",
    "abc/src/opt/lpk/lpkAbcUtil.c",
    "abc/src/opt/lpk/lpkCore.c",
    "abc/src/opt/lpk/lpkCut.c",
    "abc/src/opt/lpk/lpkMan.c",
    "abc/src/opt/lpk/lpkMap.c",
    "abc/src/opt/lpk/lpkMulti.c",
    "abc/src/opt/lpk/lpkMux.c",
    "abc/src/opt/lpk/lpkSets.c",
    "abc/src/opt/mfs/mfsCore.c",
    "abc/src/opt/mfs/mfsDiv.c",
    "abc/src/opt/mfs/mfsInter.c",
    "abc/src/opt/mfs/mfsMan.c",
    "abc/src/opt/mfs/mfsResub.c",
    "abc/src/opt/mfs/mfsSat.c",
    "abc/src/opt/mfs/mfsStrash.c",
    "abc/src/opt/mfs/mfsWin.c",
    "abc/src/opt/nwk/nwkAig.c",
    "abc/src/opt/nwk/nwkBidec.c",
    "abc/src/opt/nwk/nwkCheck.c",
    "abc/src/opt/nwk/nwkDfs.c",
    "abc/src/opt/nwk/nwkFanio.c",
    "abc/src/opt/nwk/nwkFlow.c",
    "abc/src/opt/nwk/nwkMan.c",
    "abc/src/opt/nwk/nwkMap.c",
    "abc/src/opt/nwk/nwkMerge.c",
    "abc/src/opt/nwk/nwkObj.c",
    "abc/src/opt/nwk/nwkSpeedup.c",
    "abc/src/opt/nwk/nwkStrash.c",
    "abc/src/opt/nwk/nwkTiming.c",
    "abc/src/opt/nwk/nwkUtil.c",
    "abc/src/opt/res/resCore.c",
    "abc/src/opt/res/resDivs.c",
    "abc/src/opt/res/resFilter.c",
    "abc/src/opt/res/resSat.c",
    "abc/src/opt/res/resSim.c",
    "abc/src/opt/res/resStrash.c",
    "abc/src/opt/res/resWin.c",
    "abc/src/opt/ret/retArea.c",
    "abc/src/opt/ret/retCore.c",
    "abc/src/opt/ret/retDelay.c",
    "abc/src/opt/ret/retFlow.c",
    "abc/src/opt/ret/retIncrem.c",
    "abc/src/opt/ret/retInit.c",
    "abc/src/opt/ret/retLvalue.c",
    "abc/src/opt/rwr/rwrDec.c",
    "abc/src/opt/rwr/rwrEva.c",
    "abc/src/opt/rwr/rwrExp.c",
    "abc/src/opt/rwr/rwrLib.c",
    "abc/src/opt/rwr/rwrMan.c",
    "abc/src/opt/rwr/rwrPrint.c",
    "abc/src/opt/rwr/rwrUtil.c",
    "abc/src/opt/rwt/rwtDec.c",
    "abc/src/opt/rwt/rwtMan.c",
    "abc/src/opt/rwt/rwtUtil.c",
    "abc/src/opt/sbd/sbd.c",
    "abc/src/opt/sbd/sbdCnf.c",
    "abc/src/opt/sbd/sbdCore.c",
    "abc/src/opt/sbd/sbdCut2.c",
    "abc/src/opt/sbd/sbdCut.c",
    "abc/src/opt/sbd/sbdLut.c",
    "abc/src/opt/sbd/sbdPath.c",
    "abc/src/opt/sbd/sbdSat.c",
    "abc/src/opt/sbd/sbdWin.c",
    "abc/src/opt/sfm/sfmArea.c",
    "abc/src/opt/sfm/sfmCnf.c",
    "abc/src/opt/sfm/sfmCore.c",
    "abc/src/opt/sfm/sfmDec.c",
    "abc/src/opt/sfm/sfmLib.c",
    "abc/src/opt/sfm/sfmMit.c",
    "abc/src/opt/sfm/sfmNtk.c",
    "abc/src/opt/sfm/sfmSat.c",
    "abc/src/opt/sfm/sfmTim.c",
    "abc/src/opt/sfm/sfmWin.c",
    "abc/src/opt/sim/simMan.c",
    "abc/src/opt/sim/simSeq.c",
    "abc/src/opt/sim/simSupp.c",
    "abc/src/opt/sim/simSwitch.c",
    "abc/src/opt/sim/simSym.c",
    "abc/src/opt/sim/simSymSat.c",
    "abc/src/opt/sim/simSymSim.c",
    "abc/src/opt/sim/simSymStr.c",
    "abc/src/opt/sim/simUtils.c",
    "abc/src/proof/abs/absDup.c",
    "abc/src/proof/abs/absGla.c",
    "abc/src/proof/abs/absGlaOld.c",
    "abc/src/proof/abs/absIter.c",
    "abc/src/proof/abs/absOldCex.c",
    "abc/src/proof/abs/absOldRef.c",
    "abc/src/proof/abs/absOldSat.c",
    "abc/src/proof/abs/absOldSim.c",
    "abc/src/proof/abs/absOut.c",
    "abc/src/proof/abs/absPth.c",
    "abc/src/proof/abs/absRef.c",
    "abc/src/proof/abs/absRefSelect.c",
    "abc/src/proof/abs/absRpm.c",
    "abc/src/proof/abs/absRpmOld.c",
    "abc/src/proof/abs/absUtil.c",
    "abc/src/proof/abs/absVta.c",
    "abc/src/proof/acec/acec2Mult.c",
    "abc/src/proof/acec/acecBo.c",
    "abc/src/proof/acec/acecCl.c",
    "abc/src/proof/acec/acecCo.c",
    "abc/src/proof/acec/acecCore.c",
    "abc/src/proof/acec/acecCover.c",
    "abc/src/proof/acec/acecFadds.c",
    "abc/src/proof/acec/acecMult.c",
    "abc/src/proof/acec/acecNorm.c",
    "abc/src/proof/acec/acecOrder.c",
    "abc/src/proof/acec/acecPa.c",
    "abc/src/proof/acec/acecPo.c",
    "abc/src/proof/acec/acecPolyn.c",
    "abc/src/proof/acec/acecPool.c",
    "abc/src/proof/acec/acecRe.c",
    "abc/src/proof/acec/acecSt.c",
    "abc/src/proof/acec/acecTree.c",
    "abc/src/proof/acec/acecUtil.c",
    "abc/src/proof/acec/acecXor.c",
    "abc/src/proof/cec/cecCec.c",
    "abc/src/proof/cec/cecChoice.c",
    "abc/src/proof/cec/cecClass.c",
    "abc/src/proof/cec/cecCore.c",
    "abc/src/proof/cec/cecCorr.c",
    "abc/src/proof/cec/cecIso.c",
    "abc/src/proof/cec/cecMan.c",
    "abc/src/proof/cec/cecPat.c",
    "abc/src/proof/cec/cecProve.c",
    "abc/src/proof/cec/cecSat.c",
    "abc/src/proof/cec/cecSatG2.c",
    "abc/src/proof/cec/cecSatG3.c",
    "abc/src/proof/cec/cecSatG.c",
    "abc/src/proof/cec/cecSeq.c",
    "abc/src/proof/cec/cecSim.c",
    "abc/src/proof/cec/cecSolve.c",
    "abc/src/proof/cec/cecSolveG.c",
    "abc/src/proof/cec/cecSplit.c",
    "abc/src/proof/cec/cecSweep.c",
    "abc/src/proof/cec/cecSynth.c",
    "abc/src/proof/dch/dchAig.c",
    "abc/src/proof/dch/dchChoice.c",
    "abc/src/proof/dch/dchClass.c",
    "abc/src/proof/dch/dchCnf.c",
    "abc/src/proof/dch/dchCore.c",
    "abc/src/proof/dch/dchMan.c",
    "abc/src/proof/dch/dchSat.c",
    "abc/src/proof/dch/dchSim.c",
    "abc/src/proof/dch/dchSimSat.c",
    "abc/src/proof/dch/dchSweep.c",
    "abc/src/proof/fra/fraBmc.c",
    "abc/src/proof/fra/fraCec.c",
    "abc/src/proof/fra/fraClass.c",
    "abc/src/proof/fra/fraClau.c",
    "abc/src/proof/fra/fraClaus.c",
    "abc/src/proof/fra/fraCnf.c",
    "abc/src/proof/fra/fraCore.c",
    "abc/src/proof/fra/fraHot.c",
    "abc/src/proof/fra/fraImp.c",
    "abc/src/proof/fra/fraInd.c",
    "abc/src/proof/fra/fraIndVer.c",
    "abc/src/proof/fra/fraLcr.c",
    "abc/src/proof/fra/fraMan.c",
    "abc/src/proof/fra/fraPart.c",
    "abc/src/proof/fra/fraSat.c",
    "abc/src/proof/fra/fraSec.c",
    "abc/src/proof/fra/fraSim.c",
    "abc/src/proof/fraig/fraigApi.c",
    "abc/src/proof/fraig/fraigCanon.c",
    "abc/src/proof/fraig/fraigFanout.c",
    "abc/src/proof/fraig/fraigFeed.c",
    "abc/src/proof/fraig/fraigMan.c",
    "abc/src/proof/fraig/fraigMem.c",
    "abc/src/proof/fraig/fraigNode.c",
    "abc/src/proof/fraig/fraigPrime.c",
    "abc/src/proof/fraig/fraigSat.c",
    "abc/src/proof/fraig/fraigTable.c",
    "abc/src/proof/fraig/fraigUtil.c",
    "abc/src/proof/fraig/fraigVec.c",
    "abc/src/proof/int/intCheck.c",
    "abc/src/proof/int/intContain.c",
    "abc/src/proof/int/intCore.c",
    "abc/src/proof/int/intCtrex.c",
    "abc/src/proof/int/intDup.c",
    "abc/src/proof/int/intFrames.c",
    "abc/src/proof/int/intInter.c",
    "abc/src/proof/int/intM114.c",
    "abc/src/proof/int/intMan.c",
    "abc/src/proof/int/intUtil.c",
    "abc/src/proof/live/arenaViolation.c",
    "abc/src/proof/live/combination.c",
    "abc/src/proof/live/disjunctiveMonotone.c",
    "abc/src/proof/live/kLiveConstraints.c",
    "abc/src/proof/live/kliveness.c",
    "abc/src/proof/live/liveness.c",
    "abc/src/proof/live/liveness_sim.c",
    "abc/src/proof/live/ltl_parser.c",
    "abc/src/proof/live/monotone.c",
    "abc/src/proof/pdr/pdrCnf.c",
    "abc/src/proof/pdr/pdrCore.c",
    "abc/src/proof/pdr/pdrIncr.c",
    "abc/src/proof/pdr/pdrInv.c",
    "abc/src/proof/pdr/pdrMan.c",
    "abc/src/proof/pdr/pdrSat.c",
    "abc/src/proof/pdr/pdrTsim2.c",
    "abc/src/proof/pdr/pdrTsim3.c",
    "abc/src/proof/pdr/pdrTsim.c",
    "abc/src/proof/pdr/pdrUtil.c",
    "abc/src/proof/ssc/sscClass.c",
    "abc/src/proof/ssc/sscCore.c",
    "abc/src/proof/ssc/sscSat.c",
    "abc/src/proof/ssc/sscSim.c",
    "abc/src/proof/ssc/sscUtil.c",
    "abc/src/proof/ssw/sswAig.c",
    "abc/src/proof/ssw/sswBmc.c",
    "abc/src/proof/ssw/sswClass.c",
    "abc/src/proof/ssw/sswCnf.c",
    "abc/src/proof/ssw/sswConstr.c",
    "abc/src/proof/ssw/sswCore.c",
    "abc/src/proof/ssw/sswDyn.c",
    "abc/src/proof/ssw/sswFilter.c",
    "abc/src/proof/ssw/sswIslands.c",
    "abc/src/proof/ssw/sswLcorr.c",
    "abc/src/proof/ssw/sswMan.c",
    "abc/src/proof/ssw/sswPairs.c",
    "abc/src/proof/ssw/sswPart.c",
    "abc/src/proof/ssw/sswRarity.c",
    "abc/src/proof/ssw/sswSat.c",
    "abc/src/proof/ssw/sswSemi.c",
    "abc/src/proof/ssw/sswSim.c",
    "abc/src/proof/ssw/sswSimSat.c",
    "abc/src/proof/ssw/sswSweep.c",
    "abc/src/proof/ssw/sswUnique.c",
    "abc/src/sat/bmc/bmcBCore.c",
    "abc/src/sat/bmc/bmcBmc2.c",
    "abc/src/sat/bmc/bmcBmc3.c",
    "abc/src/sat/bmc/bmcBmcAnd.c",
    "abc/src/sat/bmc/bmcBmc.c",
    "abc/src/sat/bmc/bmcBmcG.c",
    "abc/src/sat/bmc/bmcBmci.c",
    "abc/src/sat/bmc/bmcBmcS.c",
    "abc/src/sat/bmc/bmcCexCare.c",
    "abc/src/sat/bmc/bmcCexCut.c",
    "abc/src/sat/bmc/bmcCexDepth.c",
    "abc/src/sat/bmc/bmcCexMin1.c",
    "abc/src/sat/bmc/bmcCexMin2.c",
    "abc/src/sat/bmc/bmcCexTools.c",
    "abc/src/sat/bmc/bmcChain.c",
    "abc/src/sat/bmc/bmcClp.c",
    "abc/src/sat/bmc/bmcEco.c",
    "abc/src/sat/bmc/bmcExpand.c",
    "abc/src/sat/bmc/bmcFault.c",
    "abc/src/sat/bmc/bmcFx.c",
    "abc/src/sat/bmc/bmcGen.c",
    "abc/src/sat/bmc/bmcICheck.c",
    "abc/src/sat/bmc/bmcInse.c",
    "abc/src/sat/bmc/bmcLoad.c",
    "abc/src/sat/bmc/bmcMaj2.c",
    "abc/src/sat/bmc/bmcMaj3.c",
    "abc/src/sat/bmc/bmcMaj.c",
    "abc/src/sat/bmc/bmcMaxi.c",
    "abc/src/sat/bmc/bmcMesh2.c",
    "abc/src/sat/bmc/bmcMesh.c",
    "abc/src/sat/bmc/bmcMulti.c",
    "abc/src/sat/bmc/bmcUnroll.c",
    "abc/src/sat/bsat/satInterA.c",
    "abc/src/sat/bsat/satInterB.c",
    "abc/src/sat/bsat/satInter.c",
    "abc/src/sat/bsat/satInterP.c",
    "abc/src/sat/bsat/satMem.c",
    "abc/src/sat/bsat/satProof.c",
    "abc/src/sat/bsat/satSolver2.c",
    "abc/src/sat/bsat/satSolver2i.c",
    "abc/src/sat/bsat/satSolver3.c",
    "abc/src/sat/bsat/satSolver.c",
    "abc/src/sat/bsat/satStore.c",
    "abc/src/sat/bsat/satTrace.c",
    "abc/src/sat/bsat/satTruth.c",
    "abc/src/sat/bsat/satUtil.c",
    "abc/src/sat/cnf/cnfCore.c",
    "abc/src/sat/cnf/cnfCut.c",
    "abc/src/sat/cnf/cnfData.c",
    "abc/src/sat/cnf/cnfFast.c",
    "abc/src/sat/cnf/cnfMan.c",
    "abc/src/sat/cnf/cnfMap.c",
    "abc/src/sat/cnf/cnfPost.c",
    "abc/src/sat/cnf/cnfUtil.c",
    "abc/src/sat/cnf/cnfWrite.c",
    "abc/src/sat/csat/csat_apis.c",
    "abc/src/sat/glucose2/AbcGlucose2.cpp",
    "abc/src/sat/glucose2/AbcGlucoseCmd2.cpp",
    "abc/src/sat/glucose2/Glucose2.cpp",
    "abc/src/sat/glucose2/Options2.cpp",
    "abc/src/sat/glucose2/SimpSolver2.cpp",
    "abc/src/sat/glucose2/System2.cpp",
    "abc/src/sat/glucose/AbcGlucoseCmd.cpp",
    "abc/src/sat/glucose/AbcGlucose.cpp",
    "abc/src/sat/glucose/Glucose.cpp",
    "abc/src/sat/glucose/Options.cpp",
    "abc/src/sat/glucose/SimpSolver.cpp",
    "abc/src/sat/glucose/System.cpp",
    "abc/src/sat/msat/msatActivity.c",
    "abc/src/sat/msat/msatClause.c",
    "abc/src/sat/msat/msatClauseVec.c",
    "abc/src/sat/msat/msatMem.c",
    "abc/src/sat/msat/msatOrderH.c",
    "abc/src/sat/msat/msatQueue.c",
    "abc/src/sat/msat/msatRead.c",
    "abc/src/sat/msat/msatSolverApi.c",
    "abc/src/sat/msat/msatSolverCore.c",
    "abc/src/sat/msat/msatSolverIo.c",
    "abc/src/sat/msat/msatSolverSearch.c",
    "abc/src/sat/msat/msatSort.c",
    "abc/src/sat/msat/msatVec.c",
    "abc/src/sat/satoko/cnf_reader.c",
    "abc/src/sat/satoko/solver_api.c",
    "abc/src/sat/satoko/solver.c",
    "abc/src/sat/xsat/xsatCnfReader.c",
    "abc/src/sat/xsat/xsatSolverAPI.c",
    "abc/src/sat/xsat/xsatSolver.c",
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
        .define("WIN32_NO_DLL", "")
        .flag_if_supported("-Wno-unused-function")
        .flag_if_supported("-Wno-write-strings")
        .flag_if_supported("-Wno-sign-compare")
        .flag_if_supported("-Wno-alloc-size-larger-than")
        .flag_if_supported("-Wno-stringop-overflow")
        .flag_if_supported("-Wno-deprecated-declarations")
        .flag_if_supported("-Wno-format")
        .flag_if_supported("-Wno-parentheses-equality")
        .flag_if_supported("-Wno-string-plus-char")
        .include("abc/src")
        .include(".");

    let mut cc_c = cc.clone();
    let mut cc_cpp = cc;
    cc_cpp.cpp(true).std("c++17");

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
