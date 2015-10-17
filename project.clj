(defproject webvim "0.4.0-SNAPSHOT"
  :description "Toy vim clone"
  :url "http://example.com/FIXME"
  :license {:name "MIT License"
            :url ""}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [compojure "1.3.4"]
                 [hiccup "1.0.5"]
                 [ring/ring-json "0.3.1"]
                 [me.raynes/fs "1.4.6"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [ring "1.4.0"]]
  :source-paths ["src" "src/webvim"]
  :resource-paths ["thirdparty/ropes.jar", "resources"]
  :main webvim.main
  :profiles {:dev {:source-paths ["dev"]}}
  :jvm-opts ["-Dapple.awt.UIElement=true"])



