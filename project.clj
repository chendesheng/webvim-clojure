(defproject webvim "0.4.0-SNAPSHOT"
            :description "Toy vim clone"
            :url "http://example.com/FIXME"
            :license {:name "MIT License"
                      :url ""}
            :dependencies [[org.clojure/clojure "1.8.0"]
                           [cljfmt "0.3.0"]
                           [compojure "1.3.4"]
                           [hiccup "1.0.5"]
                           [ring/ring-json "0.3.1"]
                           [me.raynes/fs "1.4.6"]
                           [org.clojure/core.async "0.2.374"]
                           [info.sunng/ring-jetty9-adapter "0.9.2"]
                           [org.clojars.hozumi/clj-commons-exec "1.2.0"]
                           [http-kit "2.1.18"]
                           [clj-time "0.11.0"]
                           [org.clojure/clojurescript "1.9.36"]]
            :plugins [[lein-cljsbuild "1.1.3"]
                      [lein-figwheel "0.5.4-3"]]
            :cljsbuild {:builds [{:id "dev"
                                  :source-paths ["src-cljs" "src-cljc"]
                                  :figwheel {:on-jsload "webvim.ui.main/fig-reload"}
                                  :compiler {:main "webvim.ui.main"
                                             :asset-path "js"
                                             :source-map true
                                             :output-to "resources/public/js/cljs.js"
                                             :output-dir "resources/public/js"}}]}
            :source-paths ["src" "src-cljc"]
            :resource-paths ["thirdparty/ropes.jar", "resources"]
            :main webvim.main
            :profiles {:dev {:source-paths ["dev"]}}
            :jvm-opts [;Prevent Exceptions With “trace missing”
             ;http://theholyjava.wordpress.com/2014/05/19/clojurejava-prevent-exceptions-with-trace-missing/?utm_source=tuicool&utm_medium=referral
                       "-XX:-OmitStackTraceInFastThrow"])
