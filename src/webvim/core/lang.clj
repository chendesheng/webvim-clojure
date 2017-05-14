(ns webvim.core.lang
  (:use webvim.core.event))
;different language use differnt indent-pos, word-re etc.

(defmulti indent-pos (fn [{lang :language}] (lang :id)))
(defmulti indent-trigger? (fn [lang keycode] (lang :id)))
(defmulti word-re :id)

(defmulti init-file-type :ext)

(defmethod word-re :default [lang]
  (let [word-chars "\\w"
        space-chars "\\s"]
    {:word-chars word-chars
     :not-word-chars (str "^" word-chars)
     :space-chars space-chars
     :not-space-chars (str "^" space-chars)
     :punctuation-chars (str "^" word-chars space-chars)
     :not-punctuation-chars (str word-chars space-chars)}))

(defmethod init-file-type :default
  [buf]
  (-> buf
      (assoc-in [:language :id] ::plain-text)
      (assoc-in [:language :name] "Plain Text")
      (assoc :tabsize 4)
      (assoc :expandtab false)))

(defn- init-markdown-file-type [buf]
  (-> buf
      (assoc-in [:language :id] ::markdown)
      (assoc-in [:language :name] "MarkDown")
      (assoc :tabsize 4)
      (assoc :expandtab true)))

(defn- init-actionscript-file-type [buf]
  (-> buf
      (assoc-in [:language :id] ::actionscript)
      (assoc-in [:language :name] "ActionScript")
      (assoc :tabsize 4)
      (assoc :expandtab true)))

(defn- init-json-file-type [buf]
  (-> buf
      (assoc-in [:language :id] ::json)
      (assoc-in [:language :name] "JSON")
      (assoc :tabsize 4)
      (assoc :expandtab true)))

(defn- init-yaml-file-type [buf]
  (-> buf
      (assoc-in [:language :id] ::yaml)
      (assoc-in [:language :name] "YAML")
      (assoc :tabsize 4)
      (assoc :expandtab true)))

(defn- init-file-type-by
  ([buf id name tabsize expandtab]
    (-> buf
        (assoc-in [:language :id] ::yaml)
        (assoc-in [:language :name] "YAML")
        (assoc :tabsize 4)
        (assoc :expandtab true)))
  ([buf id name tabsize]
    (init-file-type-by buf id name tabsize true))
  ([buf id name]
    (init-file-type-by buf id name 4 true)))

(defmethod init-file-type ".cs"
  [buf]
  (init-file-type-by buf ::csharp "CSharp"))

(defmethod init-file-type ".fs"
  [buf]
  (init-file-type-by buf ::fsharp "FSharp"))

(defmethod init-file-type ".hs"
  [buf]
  (init-file-type-by buf ::haskell "Haskell"))

(defmethod init-file-type ".erlang"
  [buf]
  (init-file-type-by buf ::erlang "Erlang"))

(defmethod init-file-type ".py"
  [buf]
  (init-file-type-by buf ::python "Python"))

(defmethod init-file-type ".json"
  [buf]
  (init-json-file-type buf))

(defmethod init-file-type ".yaml"
  [buf]
  (init-yaml-file-type buf))

(defmethod init-file-type ".md"
  [buf]
  (init-markdown-file-type buf))

(defmethod init-file-type ".as"
  [buf]
  (init-actionscript-file-type buf))

