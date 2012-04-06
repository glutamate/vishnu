module Main where


import Vishnu.Lib.RunVis
import Vishnu.Lib.Testing

import Vishnu.Cmds.Pull


main = runVis [("pull", pull),
               ("showconf", showConf),
               ("status", status),
               ("build", build),
               ("diff", diff),
               ("test", test) ]