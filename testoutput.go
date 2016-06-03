package main
//^^^^^ keyword.control.go source.go
//     ^^^^^^  source.go

//  source.go
import (
//^^^^ keyword.control.import.go source.go
//     ^ meta.group.go punctuation.definition.group.begin.go source.go
//      ^  meta.import.go meta.group.go source.go
    "log"
//^^  meta.import.go meta.group.go source.go
//  ^ punctuation.definition.string.begin.go meta.import.go source.go
//   ^^^  string.quoted.double.go source.go
//      ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//       ^  meta.import.go meta.group.go source.go
    "net/http"
//^^  meta.import.go meta.group.go source.go
//  ^ punctuation.definition.string.begin.go meta.import.go source.go
//   ^^^^^^^^  string.quoted.double.go source.go
//           ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//            ^  meta.import.go meta.group.go source.go
    "net/http/cookiejar"
//^^  meta.import.go meta.group.go source.go
//  ^ punctuation.definition.string.begin.go meta.import.go source.go
//   ^^^^^^^^^^^^^^^^^^  string.quoted.double.go source.go
//                     ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//                      ^  meta.import.go meta.group.go source.go
    "os"
//^^  meta.import.go meta.group.go source.go
//  ^ punctuation.definition.string.begin.go meta.import.go source.go
//   ^^  string.quoted.double.go source.go
//     ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//      ^  meta.import.go meta.group.go source.go
    "path"
//^^  meta.import.go meta.group.go source.go
//  ^ punctuation.definition.string.begin.go meta.import.go source.go
//   ^^^^  string.quoted.double.go source.go
//       ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//        ^  meta.import.go meta.group.go source.go
    "runtime"
//^^  meta.import.go meta.group.go source.go
//  ^ punctuation.definition.string.begin.go meta.import.go source.go
//   ^^^^^^^  string.quoted.double.go source.go
//          ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//           ^  meta.import.go meta.group.go source.go
    "time"
//^^  meta.import.go meta.group.go source.go
//  ^ punctuation.definition.string.begin.go meta.import.go source.go
//   ^^^^  string.quoted.double.go source.go
//       ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//        ^  meta.import.go meta.group.go source.go
    "vger/dbHelper"
//^^  meta.import.go meta.group.go source.go
//  ^ punctuation.definition.string.begin.go meta.import.go source.go
//   ^^^^^^^^^^^^^  string.quoted.double.go source.go
//                ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//                 ^  meta.import.go meta.group.go source.go
    "vger/download"
//^^  meta.import.go meta.group.go source.go
//  ^ punctuation.definition.string.begin.go meta.import.go source.go
//   ^^^^^^^^^^^^^  string.quoted.double.go source.go
//                ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//                 ^  meta.import.go meta.group.go source.go
    "vger/filelock"
//^^  meta.import.go meta.group.go source.go
//  ^ punctuation.definition.string.begin.go meta.import.go source.go
//   ^^^^^^^^^^^^^  string.quoted.double.go source.go
//                ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//                 ^  meta.import.go meta.group.go source.go
    "vger/logger"
//^^  meta.import.go meta.group.go source.go
//  ^ punctuation.definition.string.begin.go meta.import.go source.go
//   ^^^^^^^^^^^  string.quoted.double.go source.go
//              ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//               ^  meta.import.go meta.group.go source.go
    "vger/subscribe"
//^^  meta.import.go meta.group.go source.go
//  ^ punctuation.definition.string.begin.go meta.import.go source.go
//   ^^^^^^^^^^^^^^  string.quoted.double.go source.go
//                 ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//                  ^  meta.import.go meta.group.go source.go
    "vger/thunder"
//^^  meta.import.go meta.group.go source.go
//  ^ punctuation.definition.string.begin.go meta.import.go source.go
//   ^^^^^^^^^^^^  string.quoted.double.go source.go
//               ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//                ^  meta.import.go meta.group.go source.go
    "vger/util"
//^^  meta.import.go meta.group.go source.go
//  ^ punctuation.definition.string.begin.go meta.import.go source.go
//   ^^^^^^^^^  string.quoted.double.go source.go
//            ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//             ^  meta.import.go meta.group.go source.go
)
// meta.group.go punctuation.definition.group.end.go meta.import.go source.go
//^  source.go

//  source.go
func init() {
//^^ storage.type.go source.go
//   ^^^^ entity.name.function.go source.go
//^^^^^^^ meta.function.declaration.go source.go
//        meta.function.go source.go
//       ^ meta.function.parameters.go meta.group.go punctuation.definition.group.begin.go source.go
//         meta.function.parameters.go meta.group.go source.go
//        ^ meta.function.parameters.go meta.group.go punctuation.definition.group.end.go source.go
//         ^ meta.function.go source.go
//           meta.function.go source.go
//          ^ meta.function.go meta.block.go punctuation.definition.block.begin.go source.go
//           ^  meta.function.go meta.block.go source.go
    runtime.GOMAXPROCS(runtime.NumCPU() - 1)
//^^^^^^^^^  meta.function.go meta.block.go source.go
//         ^ punctuation.accessor.go source.go
//          ^^^^^^^^^^ variable.function.go source.go
//                    ^ meta.group.go punctuation.definition.group.begin.go source.go
//                     ^^^^^^^  meta.function-call.method.go meta.group.go source.go
//                            ^ punctuation.accessor.go meta.function-call.method.go source.go
//                             ^^^^^^ variable.function.go meta.function-call.method.go source.go
//                                   ^ meta.group.go punctuation.definition.group.begin.go meta.function-call.method.go source.go
//                                    ^ meta.group.go punctuation.definition.group.end.go meta.function-call.method.go source.go
//                                     ^  meta.function-call.method.go meta.group.go source.go
//                                      ^ keyword.operator.go meta.function-call.method.go source.go
//                                       ^  meta.function-call.method.go meta.group.go source.go
//                                        ^ constant.numeric.go meta.function-call.method.go source.go
//                                         ^ meta.group.go punctuation.definition.group.end.go meta.function-call.method.go source.go
//                                          ^  meta.function.go meta.block.go source.go
    err := os.Chdir(path.Dir(os.Args[0]))
//^^  meta.function.go meta.block.go source.go
//  ^^^ variable.other.go source.go
//     ^  meta.initialization.short.go source.go
//      ^^ keyword.operator.initialize.go meta.initialization.short.go source.go
//        ^^^  meta.function.go meta.block.go source.go
//           ^ punctuation.accessor.go source.go
//            ^^^^^ variable.function.go source.go
//                 ^ meta.group.go punctuation.definition.group.begin.go source.go
//                  ^^^^  meta.function-call.method.go meta.group.go source.go
//                      ^ punctuation.accessor.go meta.function-call.method.go source.go
//                       ^^^ variable.function.go meta.function-call.method.go source.go
//                          ^ meta.group.go punctuation.definition.group.begin.go meta.function-call.method.go source.go
//                           ^^  meta.function-call.method.go meta.group.go source.go
//                             ^ punctuation.accessor.go meta.function-call.method.go source.go
//                              ^^^^ variable.other.dot-access.go meta.function-call.method.go source.go
//                                  ^ punctuation.definition.brackets.begin.go meta.function-call.method.go source.go
//                                   ^ constant.numeric.go meta.brackets.go source.go
//                                    ^ punctuation.definition.brackets.end.go meta.brackets.go source.go
//                                     ^ meta.group.go punctuation.definition.group.end.go meta.function-call.method.go source.go
//                                      ^ meta.group.go punctuation.definition.group.end.go meta.function-call.method.go source.go
//                                       ^  meta.function.go meta.block.go source.go
    if err != nil {
//^^  meta.function.go meta.block.go source.go
//  ^^ keyword.control.go source.go
//    ^^^^^  meta.function.go meta.block.go source.go
//         ^^ keyword.operator.go source.go
//           ^  meta.function.go meta.block.go source.go
//            ^^^ constant.language.go source.go
//               ^  meta.function.go meta.block.go source.go
//                ^ punctuation.definition.block.begin.go source.go
//                 ^  meta.block.go source.go
        log.Fatal(err)
//^^^^^^^^^  meta.block.go source.go
//         ^ punctuation.accessor.go meta.block.go source.go
//          ^^^^^ variable.function.go meta.block.go source.go
//               ^ meta.group.go punctuation.definition.group.begin.go meta.block.go source.go
//                ^^^  meta.function-call.method.go meta.group.go source.go
//                   ^ meta.group.go punctuation.definition.group.end.go meta.function-call.method.go source.go
//                    ^  meta.block.go source.go
    }
//^^  meta.block.go source.go
//  ^ punctuation.definition.block.end.go meta.block.go source.go
//   ^  meta.function.go meta.block.go source.go

//  meta.function.go meta.block.go source.go
    logbase := util.ReadConfig("log")
//^^  meta.function.go meta.block.go source.go
//  ^^^^^^^ variable.other.go source.go
//         ^  meta.initialization.short.go source.go
//          ^^ keyword.operator.initialize.go meta.initialization.short.go source.go
//            ^^^^^  meta.function.go meta.block.go source.go
//                 ^ punctuation.accessor.go source.go
//                  ^^^^^^^^^^ variable.function.go source.go
//                            ^ meta.group.go punctuation.definition.group.begin.go source.go
//                             ^ punctuation.definition.string.begin.go meta.function-call.method.go source.go
//                              ^^^  string.quoted.double.go source.go
//                                 ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//                                  ^ meta.group.go punctuation.definition.group.end.go meta.function-call.method.go source.go
//                                   ^  meta.function.go meta.block.go source.go
    logger.InitLog("[V'ger]", path.Join(logbase, "vger.log"))
//^^^^^^^^  meta.function.go meta.block.go source.go
//        ^ punctuation.accessor.go source.go
//         ^^^^^^^ variable.function.go source.go
//                ^ meta.group.go punctuation.definition.group.begin.go source.go
//                 ^ punctuation.definition.string.begin.go meta.function-call.method.go source.go
//                  ^^^^^^^  string.quoted.double.go source.go
//                         ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//                          ^^^^^^  meta.function-call.method.go meta.group.go source.go
//                                ^ punctuation.accessor.go meta.function-call.method.go source.go
//                                 ^^^^ variable.function.go meta.function-call.method.go source.go
//                                     ^ meta.group.go punctuation.definition.group.begin.go meta.function-call.method.go source.go
//                                      ^^^^^^^^^  meta.function-call.method.go meta.group.go source.go
//                                               ^ punctuation.definition.string.begin.go meta.function-call.method.go source.go
//                                                ^^^^^^^^  string.quoted.double.go source.go
//                                                        ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//                                                         ^ meta.group.go punctuation.definition.group.end.go meta.function-call.method.go source.go
//                                                          ^ meta.group.go punctuation.definition.group.end.go meta.function-call.method.go source.go
//                                                           ^  meta.function.go meta.block.go source.go

//  meta.function.go meta.block.go source.go
    jar, _ := cookiejar.New(nil)
//^^  meta.function.go meta.block.go source.go
//  ^^^ variable.other.go source.go
//     ^ punctuation.separator.go meta.initialization.short.go source.go
//       ^ variable.other.go meta.initialization.short.go source.go
//        ^  meta.initialization.short.go source.go
//         ^^ keyword.operator.initialize.go meta.initialization.short.go source.go
//           ^^^^^^^^^^  meta.function.go meta.block.go source.go
//                     ^ punctuation.accessor.go source.go
//                      ^^^ variable.function.go source.go
//                         ^ meta.group.go punctuation.definition.group.begin.go source.go
//                          ^^^ constant.language.go meta.function-call.method.go source.go
//                             ^ meta.group.go punctuation.definition.group.end.go meta.function-call.method.go source.go
//                              ^  meta.function.go meta.block.go source.go
    http.DefaultClient.Jar = jar
//^^^^^^  meta.function.go meta.block.go source.go
//      ^ punctuation.accessor.go source.go
//       ^^^^^^^^^^^^^ variable.other.dot-access.go source.go
//                    ^ punctuation.accessor.go source.go
//                     ^^^ variable.other.dot-access.go source.go
//                        ^  meta.function.go meta.block.go source.go
//                         ^ keyword.operator.assignment.go source.go
//                          ^^^^^  meta.function.go meta.block.go source.go
    //http.DefaultClient.Jar, _ = nativejar.New()
//^^  meta.function.go meta.block.go source.go
//  ^^ punctuation.definition.comment.go source.go
//    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  comment.line.double-slash.go source.go
//                                                  meta.function.go meta.block.go source.go

//  meta.function.go meta.block.go source.go
    util.SaveConfig("shutdown-after-finish", "false")
//^^^^^^  meta.function.go meta.block.go source.go
//      ^ punctuation.accessor.go source.go
//       ^^^^^^^^^^ variable.function.go source.go
//                 ^ meta.group.go punctuation.definition.group.begin.go source.go
//                  ^ punctuation.definition.string.begin.go meta.function-call.method.go source.go
//                   ^^^^^^^^^^^^^^^^^^^^^  string.quoted.double.go source.go
//                                        ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//                                         ^^  meta.function-call.method.go meta.group.go source.go
//                                           ^ punctuation.definition.string.begin.go meta.function-call.method.go source.go
//                                            ^^^^^  string.quoted.double.go source.go
//                                                 ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//                                                  ^ meta.group.go punctuation.definition.group.end.go meta.function-call.method.go source.go
//                                                   ^  meta.function.go meta.block.go source.go

//  meta.function.go meta.block.go source.go
    //set timeout
//^^  meta.function.go meta.block.go source.go
//  ^^ punctuation.definition.comment.go source.go
//    ^^^^^^^^^^^  comment.line.double-slash.go source.go
//                  meta.function.go meta.block.go source.go
    networkTimeout := time.Duration(util.ReadIntConfig("network-timeout")) * time.Second
//^^  meta.function.go meta.block.go source.go
//  ^^^^^^^^^^^^^^ variable.other.go source.go
//                ^  meta.initialization.short.go source.go
//                 ^^ keyword.operator.initialize.go meta.initialization.short.go source.go
//                   ^^^^^  meta.function.go meta.block.go source.go
//                        ^ punctuation.accessor.go source.go
//                         ^^^^^^^^ variable.function.go source.go
//                                 ^ meta.group.go punctuation.definition.group.begin.go source.go
//                                  ^^^^  meta.function-call.method.go meta.group.go source.go
//                                      ^ punctuation.accessor.go meta.function-call.method.go source.go
//                                       ^^^^^^^^^^^^^ variable.function.go meta.function-call.method.go source.go
//                                                    ^ meta.group.go punctuation.definition.group.begin.go meta.function-call.method.go source.go
//                                                     ^ punctuation.definition.string.begin.go meta.function-call.method.go source.go
//                                                      ^^^^^^^^^^^^^^^  string.quoted.double.go source.go
//                                                                     ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//                                                                      ^ meta.group.go punctuation.definition.group.end.go meta.function-call.method.go source.go
//                                                                       ^ meta.group.go punctuation.definition.group.end.go meta.function-call.method.go source.go
//                                                                        ^  meta.function.go meta.block.go source.go
//                                                                         ^ keyword.operator.go source.go
//                                                                          ^^^^^  meta.function.go meta.block.go source.go
//                                                                               ^ punctuation.accessor.go source.go
//                                                                                ^^^^^^ variable.other.dot-access.go source.go
//                                                                                      ^  meta.function.go meta.block.go source.go
    transport := http.DefaultTransport.(*http.Transport)
//^^  meta.function.go meta.block.go source.go
//  ^^^^^^^^^ variable.other.go source.go
//           ^  meta.initialization.short.go source.go
//            ^^ keyword.operator.initialize.go meta.initialization.short.go source.go
//              ^^^^^  meta.function.go meta.block.go source.go
//                   ^ punctuation.accessor.go source.go
//                    ^^^^^^^^^^^^^^^^ variable.other.dot-access.go source.go
//                                    ^  meta.function.go meta.block.go source.go
//                                     ^ punctuation.definition.group.begin.go source.go
//                                      ^ keyword.operator.go meta.group.go source.go
//                                       ^^^^  meta.group.go source.go
//                                           ^ punctuation.accessor.go meta.group.go source.go
//                                            ^^^^^^^^^ variable.other.dot-access.go meta.group.go source.go
//                                                     ^ punctuation.definition.group.end.go meta.group.go source.go
//                                                      ^  meta.function.go meta.block.go source.go
    transport.ResponseHeaderTimeout = networkTimeout
//^^^^^^^^^^^  meta.function.go meta.block.go source.go
//           ^ punctuation.accessor.go source.go
//            ^^^^^^^^^^^^^^^^^^^^^ variable.other.dot-access.go source.go
//                                 ^  meta.function.go meta.block.go source.go
//                                  ^ keyword.operator.assignment.go source.go
//                                   ^^^^^^^^^^^^^^^^  meta.function.go meta.block.go source.go
    transport.MaxIdleConnsPerHost = 3
//^^^^^^^^^^^  meta.function.go meta.block.go source.go
//           ^ punctuation.accessor.go source.go
//            ^^^^^^^^^^^^^^^^^^^ variable.other.dot-access.go source.go
//                               ^  meta.function.go meta.block.go source.go
//                                ^ keyword.operator.assignment.go source.go
//                                 ^  meta.function.go meta.block.go source.go
//                                  ^ constant.numeric.go source.go
//                                   ^  meta.function.go meta.block.go source.go

//  meta.function.go meta.block.go source.go
    thunder.UserName = util.ReadConfig("thunder-user")
//^^^^^^^^^  meta.function.go meta.block.go source.go
//         ^ punctuation.accessor.go source.go
//          ^^^^^^^^ variable.other.dot-access.go source.go
//                  ^  meta.function.go meta.block.go source.go
//                   ^ keyword.operator.assignment.go source.go
//                    ^^^^^  meta.function.go meta.block.go source.go
//                         ^ punctuation.accessor.go source.go
//                          ^^^^^^^^^^ variable.function.go source.go
//                                    ^ meta.group.go punctuation.definition.group.begin.go source.go
//                                     ^ punctuation.definition.string.begin.go meta.function-call.method.go source.go
//                                      ^^^^^^^^^^^^  string.quoted.double.go source.go
//                                                  ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//                                                   ^ meta.group.go punctuation.definition.group.end.go meta.function-call.method.go source.go
//                                                    ^  meta.function.go meta.block.go source.go
    thunder.Password = util.ReadConfig("thunder-password")
//^^^^^^^^^  meta.function.go meta.block.go source.go
//         ^ punctuation.accessor.go source.go
//          ^^^^^^^^ variable.other.dot-access.go source.go
//                  ^  meta.function.go meta.block.go source.go
//                   ^ keyword.operator.assignment.go source.go
//                    ^^^^^  meta.function.go meta.block.go source.go
//                         ^ punctuation.accessor.go source.go
//                          ^^^^^^^^^^ variable.function.go source.go
//                                    ^ meta.group.go punctuation.definition.group.begin.go source.go
//                                     ^ punctuation.definition.string.begin.go meta.function-call.method.go source.go
//                                      ^^^^^^^^^^^^^^^^  string.quoted.double.go source.go
//                                                      ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//                                                       ^ meta.group.go punctuation.definition.group.end.go meta.function-call.method.go source.go
//                                                        ^  meta.function.go meta.block.go source.go
    thunder.Gdriveid = util.ReadConfig("gdriveid")
//^^^^^^^^^  meta.function.go meta.block.go source.go
//         ^ punctuation.accessor.go source.go
//          ^^^^^^^^ variable.other.dot-access.go source.go
//                  ^  meta.function.go meta.block.go source.go
//                   ^ keyword.operator.assignment.go source.go
//                    ^^^^^  meta.function.go meta.block.go source.go
//                         ^ punctuation.accessor.go source.go
//                          ^^^^^^^^^^ variable.function.go source.go
//                                    ^ meta.group.go punctuation.definition.group.begin.go source.go
//                                     ^ punctuation.definition.string.begin.go meta.function-call.method.go source.go
//                                      ^^^^^^^^  string.quoted.double.go source.go
//                                              ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//                                               ^ meta.group.go punctuation.definition.group.end.go meta.function-call.method.go source.go
//                                                ^  meta.function.go meta.block.go source.go

//  meta.function.go meta.block.go source.go
    formats := util.ReadStringSliceConfig("yyets-formats")
//^^  meta.function.go meta.block.go source.go
//  ^^^^^^^ variable.other.go source.go
//         ^  meta.initialization.short.go source.go
//          ^^ keyword.operator.initialize.go meta.initialization.short.go source.go
//            ^^^^^  meta.function.go meta.block.go source.go
//                 ^ punctuation.accessor.go source.go
//                  ^^^^^^^^^^^^^^^^^^^^^ variable.function.go source.go
//                                       ^ meta.group.go punctuation.definition.group.begin.go source.go
//                                        ^ punctuation.definition.string.begin.go meta.function-call.method.go source.go
//                                         ^^^^^^^^^^^^^  string.quoted.double.go source.go
//                                                      ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//                                                       ^ meta.group.go punctuation.definition.group.end.go meta.function-call.method.go source.go
//                                                        ^  meta.function.go meta.block.go source.go
    for _, ft := range formats {
//^^  meta.function.go meta.block.go source.go
//  ^^^ keyword.control.go source.go
//     ^  meta.function.go meta.block.go source.go
//      ^ variable.other.go source.go
//       ^ punctuation.separator.go meta.initialization.short.go source.go
//         ^^ variable.other.go meta.initialization.short.go source.go
//           ^  meta.initialization.short.go source.go
//            ^^ keyword.operator.initialize.go meta.initialization.short.go source.go
//              ^  meta.function.go meta.block.go source.go
//               ^^^^^ keyword.control.go source.go
//                    ^^^^^^^^^  meta.function.go meta.block.go source.go
//                             ^ punctuation.definition.block.begin.go source.go
//                              ^  meta.block.go source.go
        subscribe.YYetsFormats[ft] = struct{}{}
//^^^^^^^^^^^^^^^  meta.block.go source.go
//               ^ punctuation.accessor.go meta.block.go source.go
//                ^^^^^^^^^^^^ variable.other.dot-access.go meta.block.go source.go
//                            ^ punctuation.definition.brackets.begin.go meta.block.go source.go
//                             ^^  meta.brackets.go source.go
//                               ^ punctuation.definition.brackets.end.go meta.brackets.go source.go
//                                ^  meta.block.go source.go
//                                 ^ keyword.operator.assignment.go meta.block.go source.go
//                                  ^  meta.block.go source.go
//                                   ^^^^^^ storage.type.go meta.block.go source.go
//                                         ^ meta.block.go punctuation.definition.block.begin.go meta.block.go source.go
//                                          ^ meta.block.go punctuation.definition.block.end.go meta.block.go source.go
//                                           ^ punctuation.definition.block.begin.go meta.block.go source.go
//                                            ^ punctuation.definition.block.end.go meta.block.go source.go
//                                             ^  meta.block.go source.go
    }
//^^  meta.block.go source.go
//  ^ punctuation.definition.block.end.go meta.block.go source.go
//   ^  meta.function.go meta.block.go source.go
    log.Printf("yyets-formats:%v", subscribe.YYetsFormats)
//^^^^^  meta.function.go meta.block.go source.go
//     ^ punctuation.accessor.go source.go
//      ^^^^^^ variable.function.go source.go
//            ^ meta.group.go punctuation.definition.group.begin.go source.go
//             ^ punctuation.definition.string.begin.go meta.function-call.method.go source.go
//              ^^^^^^^^^^^^^^  string.quoted.double.go source.go
//                            ^^ constant.other.placeholder.go string.quoted.double.go source.go
//                              ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//                               ^^^^^^^^^^^  meta.function-call.method.go meta.group.go source.go
//                                          ^ punctuation.accessor.go meta.function-call.method.go source.go
//                                           ^^^^^^^^^^^^ variable.other.dot-access.go meta.function-call.method.go source.go
//                                                       ^ meta.group.go punctuation.definition.group.end.go meta.function-call.method.go source.go
//                                                        ^  meta.function.go meta.block.go source.go

//  meta.function.go meta.block.go source.go
    go func() {
//^^  meta.function.go meta.block.go source.go
//  ^^ keyword.control.go source.go
//    ^  meta.function.go meta.block.go source.go
//     ^^^^ meta.function.declaration.anonymous.go storage.type.go source.go
//          meta.function.go source.go
//         ^^ meta.function.parameters.go meta.group.go source.go
//         ^ punctuation.definition.group.begin.go source.go
//          ^ punctuation.definition.group.end.go source.go
//           ^ meta.function.go source.go
//             meta.function.go source.go
//            ^ meta.function.go meta.block.go punctuation.definition.block.begin.go source.go
//             ^  meta.function.go meta.block.go source.go
        err := thunder.Login(nil)
//^^^^^^  meta.function.go meta.block.go source.go
//      ^^^ variable.other.go source.go
//         ^  meta.initialization.short.go source.go
//          ^^ keyword.operator.initialize.go meta.initialization.short.go source.go
//            ^^^^^^^^  meta.function.go meta.block.go source.go
//                    ^ punctuation.accessor.go source.go
//                     ^^^^^ variable.function.go source.go
//                          ^ meta.group.go punctuation.definition.group.begin.go source.go
//                           ^^^ constant.language.go meta.function-call.method.go source.go
//                              ^ meta.group.go punctuation.definition.group.end.go meta.function-call.method.go source.go
//                               ^  meta.function.go meta.block.go source.go
        if err != nil {
//^^^^^^  meta.function.go meta.block.go source.go
//      ^^ keyword.control.go source.go
//        ^^^^^  meta.function.go meta.block.go source.go
//             ^^ keyword.operator.go source.go
//               ^  meta.function.go meta.block.go source.go
//                ^^^ constant.language.go source.go
//                   ^  meta.function.go meta.block.go source.go
//                    ^ punctuation.definition.block.begin.go source.go
//                     ^  meta.block.go source.go
            log.Print(err)
//^^^^^^^^^^^^^  meta.block.go source.go
//             ^ punctuation.accessor.go meta.block.go source.go
//              ^^^^^ variable.function.go meta.block.go source.go
//                   ^ meta.group.go punctuation.definition.group.begin.go meta.block.go source.go
//                    ^^^  meta.function-call.method.go meta.group.go source.go
//                       ^ meta.group.go punctuation.definition.group.end.go meta.function-call.method.go source.go
//                        ^  meta.block.go source.go
        }
//^^^^^^  meta.block.go source.go
//      ^ punctuation.definition.block.end.go meta.block.go source.go
//       ^  meta.function.go meta.block.go source.go
    }()
//^^  meta.function.go meta.block.go source.go
//  ^ meta.function.go meta.block.go punctuation.definition.block.end.go source.go
//   ^ punctuation.definition.group.begin.go source.go
//    ^ punctuation.definition.group.end.go meta.group.go source.go
//     ^  meta.function.go meta.block.go source.go

//  meta.function.go meta.block.go source.go
    dbHelper.Init("sqlite3", path.Join(util.ReadConfig("dir"), "vger.db"))
//^^^^^^^^^^  meta.function.go meta.block.go source.go
//          ^ punctuation.accessor.go source.go
//           ^^^^ variable.function.go source.go
//               ^ meta.group.go punctuation.definition.group.begin.go source.go
//                ^ punctuation.definition.string.begin.go meta.function-call.method.go source.go
//                 ^^^^^^^  string.quoted.double.go source.go
//                        ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//                         ^^^^^^  meta.function-call.method.go meta.group.go source.go
//                               ^ punctuation.accessor.go meta.function-call.method.go source.go
//                                ^^^^ variable.function.go meta.function-call.method.go source.go
//                                    ^ meta.group.go punctuation.definition.group.begin.go meta.function-call.method.go source.go
//                                     ^^^^  meta.function-call.method.go meta.group.go source.go
//                                         ^ punctuation.accessor.go meta.function-call.method.go source.go
//                                          ^^^^^^^^^^ variable.function.go meta.function-call.method.go source.go
//                                                    ^ meta.group.go punctuation.definition.group.begin.go meta.function-call.method.go source.go
//                                                     ^ punctuation.definition.string.begin.go meta.function-call.method.go source.go
//                                                      ^^^  string.quoted.double.go source.go
//                                                         ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//                                                          ^ meta.group.go punctuation.definition.group.end.go meta.function-call.method.go source.go
//                                                           ^^  meta.function-call.method.go meta.group.go source.go
//                                                             ^ punctuation.definition.string.begin.go meta.function-call.method.go source.go
//                                                              ^^^^^^^  string.quoted.double.go source.go
//                                                                     ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//                                                                      ^ meta.group.go punctuation.definition.group.end.go meta.function-call.method.go source.go
//                                                                       ^ meta.group.go punctuation.definition.group.end.go meta.function-call.method.go source.go
//                                                                        ^  meta.function.go meta.block.go source.go

//  meta.function.go meta.block.go source.go
    filelock.DefaultLock, _ = filelock.New("/tmp/vger.db.lock.txt")
//^^^^^^^^^^  meta.function.go meta.block.go source.go
//          ^ punctuation.accessor.go source.go
//           ^^^^^^^^^^^ variable.other.dot-access.go source.go
//                      ^^^^  meta.function.go meta.block.go source.go
//                          ^ keyword.operator.assignment.go source.go
//                           ^^^^^^^^^  meta.function.go meta.block.go source.go
//                                    ^ punctuation.accessor.go source.go
//                                     ^^^ variable.function.go source.go
//                                        ^ meta.group.go punctuation.definition.group.begin.go source.go
//                                         ^ punctuation.definition.string.begin.go meta.function-call.method.go source.go
//                                          ^^^^^^^^^^^^^^^^^^^^^  string.quoted.double.go source.go
//                                                               ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//                                                                ^ meta.group.go punctuation.definition.group.end.go meta.function-call.method.go source.go
//                                                                 ^  meta.function.go meta.block.go source.go

//  meta.function.go meta.block.go source.go
    download.BaseDir = util.ReadConfig("dir")
//^^^^^^^^^^  meta.function.go meta.block.go source.go
//          ^ punctuation.accessor.go source.go
//           ^^^^^^^ variable.other.dot-access.go source.go
//                  ^  meta.function.go meta.block.go source.go
//                   ^ keyword.operator.assignment.go source.go
//                    ^^^^^  meta.function.go meta.block.go source.go
//                         ^ punctuation.accessor.go source.go
//                          ^^^^^^^^^^ variable.function.go source.go
//                                    ^ meta.group.go punctuation.definition.group.begin.go source.go
//                                     ^ punctuation.definition.string.begin.go meta.function-call.method.go source.go
//                                      ^^^  string.quoted.double.go source.go
//                                         ^ punctuation.definition.string.end.go string.quoted.double.go source.go
//                                          ^ meta.group.go punctuation.definition.group.end.go meta.function-call.method.go source.go
//                                           ^  meta.function.go meta.block.go source.go
    download.NetworkTimeout = networkTimeout
//^^^^^^^^^^  meta.function.go meta.block.go source.go
//          ^ punctuation.accessor.go source.go
//           ^^^^^^^^^^^^^^ variable.other.dot-access.go source.go
//                         ^  meta.function.go meta.block.go source.go
//                          ^ keyword.operator.assignment.go source.go
//                           ^^^^^^^^^^^^^^^^  meta.function.go meta.block.go source.go
}
// meta.function.go meta.block.go punctuation.definition.block.end.go source.go
//^  source.go
