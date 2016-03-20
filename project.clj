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
                           [http-kit "2.1.18"]]
            :source-paths ["src" "src/webvim"]
            :resource-paths ["thirdparty/ropes.jar", "resources"]
            :main webvim.main
            :profiles {:dev {:source-paths ["dev"]}}
            :jvm-opts ["-Dapple.awt.UIElement=true" 
             ;Prevent Exceptions With “trace missing”
             ;http://theholyjava.wordpress.com/2014/05/19/clojurejava-prevent-exceptions-with-trace-missing/?utm_source=tuicool&utm_medium=referral
                       "-XX:-OmitStackTraceInFastThrow"])



