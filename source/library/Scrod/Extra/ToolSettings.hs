module Scrod.Extra.ToolSettings where

import qualified GHC.Settings as Settings

emptyToolSettings :: Settings.ToolSettings
emptyToolSettings =
  Settings.ToolSettings
    { Settings.toolSettings_arSupportsDashL = error "ToolSettings.arSupportsDashL",
      Settings.toolSettings_ccSupportsNoPie = error "ToolSettings.ccSupportsNoPie",
      Settings.toolSettings_extraGccViaCFlags = error "ToolSettings.extraGccViaCFlags",
      Settings.toolSettings_ldIsGnuLd = error "ToolSettings.ldIsGnuLd",
      Settings.toolSettings_ldSupportsCompactUnwind = error "ToolSettings.ldSupportsCompactUnwind",
      Settings.toolSettings_ldSupportsFilelist = error "ToolSettings.ldSupportsFilelist",
      Settings.toolSettings_ldSupportsSingleModule = error "ToolSettings.ldSupportsSingleModule",
      Settings.toolSettings_mergeObjsSupportsResponseFiles = error "ToolSettings.mergeObjsSupportsResponseFiles",
      Settings.toolSettings_opt_a = error "ToolSettings.opt_a",
      Settings.toolSettings_opt_c = error "ToolSettings.opt_c",
      Settings.toolSettings_opt_cxx = error "ToolSettings.opt_cxx",
      Settings.toolSettings_opt_F = error "ToolSettings.opt_F",
      Settings.toolSettings_opt_i = error "ToolSettings.opt_i",
      Settings.toolSettings_opt_l = error "ToolSettings.opt_l",
      Settings.toolSettings_opt_L = error "ToolSettings.opt_L",
      Settings.toolSettings_opt_las = error "ToolSettings.opt_las",
      Settings.toolSettings_opt_lc = error "ToolSettings.opt_lc",
      Settings.toolSettings_opt_lm = error "ToolSettings.opt_lm",
      Settings.toolSettings_opt_lo = error "ToolSettings.opt_lo",
      Settings.toolSettings_opt_P = error "ToolSettings.opt_P",
      Settings.toolSettings_opt_P_fingerprint = error "ToolSettings.opt_P_fingerprint",
      Settings.toolSettings_opt_windres = error "ToolSettings.opt_windres",
      Settings.toolSettings_pgm_a = error "ToolSettings.pgm_a",
      Settings.toolSettings_pgm_ar = error "ToolSettings.pgm_ar",
      Settings.toolSettings_pgm_c = error "ToolSettings.pgm_c",
      Settings.toolSettings_pgm_cpp = error "ToolSettings.pgm_cpp",
      Settings.toolSettings_pgm_cxx = error "ToolSettings.pgm_cxx",
      Settings.toolSettings_pgm_F = error "ToolSettings.pgm_F",
      Settings.toolSettings_pgm_i = error "ToolSettings.pgm_i",
      Settings.toolSettings_pgm_install_name_tool = error "ToolSettings.pgm_install_name_tool",
      Settings.toolSettings_pgm_l = error "ToolSettings.pgm_l",
      Settings.toolSettings_pgm_L = error "ToolSettings.pgm_L",
      Settings.toolSettings_pgm_las = error "ToolSettings.pgm_las",
      Settings.toolSettings_pgm_lc = error "ToolSettings.pgm_lc",
      Settings.toolSettings_pgm_lm = error "ToolSettings.pgm_lm",
      Settings.toolSettings_pgm_lo = error "ToolSettings.pgm_lo",
      Settings.toolSettings_pgm_otool = error "ToolSettings.pgm_otool",
      Settings.toolSettings_pgm_P = error "ToolSettings.pgm_P",
      Settings.toolSettings_pgm_ranlib = error "ToolSettings.pgm_ranlib",
      Settings.toolSettings_pgm_windres = error "ToolSettings.pgm_windres",
      Settings.toolSettings_useInplaceMinGW = error "ToolSettings.useInplaceMinGW"
    }
