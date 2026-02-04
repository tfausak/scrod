{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Ghc.ToolSettings where

import qualified GHC.Settings as Settings
import qualified GHC.Stack as Stack
import qualified LegendaryChainsaw.Ghc.Uninitialized as Uninitialized

empty :: (Stack.HasCallStack) => Settings.ToolSettings
empty =
  Settings.ToolSettings
    { Settings.toolSettings_arSupportsDashL = Uninitialized.throw 'Settings.toolSettings_arSupportsDashL,
      Settings.toolSettings_ccSupportsNoPie = Uninitialized.throw 'Settings.toolSettings_ccSupportsNoPie,
      Settings.toolSettings_cmmCppSupportsG0 = Uninitialized.throw 'Settings.toolSettings_cmmCppSupportsG0,
      Settings.toolSettings_extraGccViaCFlags = Uninitialized.throw 'Settings.toolSettings_extraGccViaCFlags,
      Settings.toolSettings_ldIsGnuLd = Uninitialized.throw 'Settings.toolSettings_ldIsGnuLd,
      Settings.toolSettings_ldSupportsCompactUnwind = Uninitialized.throw 'Settings.toolSettings_ldSupportsCompactUnwind,
      Settings.toolSettings_ldSupportsFilelist = Uninitialized.throw 'Settings.toolSettings_ldSupportsFilelist,
      Settings.toolSettings_ldSupportsSingleModule = Uninitialized.throw 'Settings.toolSettings_ldSupportsSingleModule,
      Settings.toolSettings_mergeObjsSupportsResponseFiles = Uninitialized.throw 'Settings.toolSettings_mergeObjsSupportsResponseFiles,
      Settings.toolSettings_opt_CmmP = Uninitialized.throw 'Settings.toolSettings_opt_CmmP,
      Settings.toolSettings_opt_CmmP_fingerprint = Uninitialized.throw 'Settings.toolSettings_opt_CmmP_fingerprint,
      Settings.toolSettings_opt_F = Uninitialized.throw 'Settings.toolSettings_opt_F,
      Settings.toolSettings_opt_JSP = Uninitialized.throw 'Settings.toolSettings_opt_JSP,
      Settings.toolSettings_opt_JSP_fingerprint = Uninitialized.throw 'Settings.toolSettings_opt_JSP_fingerprint,
      Settings.toolSettings_opt_L = Uninitialized.throw 'Settings.toolSettings_opt_L,
      Settings.toolSettings_opt_P = Uninitialized.throw 'Settings.toolSettings_opt_P,
      Settings.toolSettings_opt_P_fingerprint = Uninitialized.throw 'Settings.toolSettings_opt_P_fingerprint,
      Settings.toolSettings_opt_a = Uninitialized.throw 'Settings.toolSettings_opt_a,
      Settings.toolSettings_opt_c = Uninitialized.throw 'Settings.toolSettings_opt_c,
      Settings.toolSettings_opt_cxx = Uninitialized.throw 'Settings.toolSettings_opt_cxx,
      Settings.toolSettings_opt_i = Uninitialized.throw 'Settings.toolSettings_opt_i,
      Settings.toolSettings_opt_l = Uninitialized.throw 'Settings.toolSettings_opt_l,
      Settings.toolSettings_opt_las = Uninitialized.throw 'Settings.toolSettings_opt_las,
      Settings.toolSettings_opt_lc = Uninitialized.throw 'Settings.toolSettings_opt_lc,
      Settings.toolSettings_opt_lm = Uninitialized.throw 'Settings.toolSettings_opt_lm,
      Settings.toolSettings_opt_lo = Uninitialized.throw 'Settings.toolSettings_opt_lo,
      Settings.toolSettings_opt_windres = Uninitialized.throw 'Settings.toolSettings_opt_windres,
      Settings.toolSettings_pgm_CmmP = Uninitialized.throw 'Settings.toolSettings_pgm_CmmP,
      Settings.toolSettings_pgm_F = Uninitialized.throw 'Settings.toolSettings_pgm_F,
      Settings.toolSettings_pgm_JSP = Uninitialized.throw 'Settings.toolSettings_pgm_JSP,
      Settings.toolSettings_pgm_L = Uninitialized.throw 'Settings.toolSettings_pgm_L,
      Settings.toolSettings_pgm_P = Uninitialized.throw 'Settings.toolSettings_pgm_P,
      Settings.toolSettings_pgm_a = Uninitialized.throw 'Settings.toolSettings_pgm_a,
      Settings.toolSettings_pgm_ar = Uninitialized.throw 'Settings.toolSettings_pgm_ar,
      Settings.toolSettings_pgm_c = Uninitialized.throw 'Settings.toolSettings_pgm_c,
      Settings.toolSettings_pgm_cpp = Uninitialized.throw 'Settings.toolSettings_pgm_cpp,
      Settings.toolSettings_pgm_cxx = Uninitialized.throw 'Settings.toolSettings_pgm_cxx,
      Settings.toolSettings_pgm_i = Uninitialized.throw 'Settings.toolSettings_pgm_i,
      Settings.toolSettings_pgm_install_name_tool = Uninitialized.throw 'Settings.toolSettings_pgm_install_name_tool,
      Settings.toolSettings_pgm_l = Uninitialized.throw 'Settings.toolSettings_pgm_l,
      Settings.toolSettings_pgm_las = Uninitialized.throw 'Settings.toolSettings_pgm_las,
      Settings.toolSettings_pgm_lc = Uninitialized.throw 'Settings.toolSettings_pgm_lc,
      Settings.toolSettings_pgm_lm = Uninitialized.throw 'Settings.toolSettings_pgm_lm,
      Settings.toolSettings_pgm_lo = Uninitialized.throw 'Settings.toolSettings_pgm_lo,
      Settings.toolSettings_pgm_otool = Uninitialized.throw 'Settings.toolSettings_pgm_otool,
      Settings.toolSettings_pgm_ranlib = Uninitialized.throw 'Settings.toolSettings_pgm_ranlib,
      Settings.toolSettings_pgm_windres = Uninitialized.throw 'Settings.toolSettings_pgm_windres,
      Settings.toolSettings_useInplaceMinGW = Uninitialized.throw 'Settings.toolSettings_useInplaceMinGW
    }
