(ns webvim.syntax
  "sublime-syntax parser"
  (:require [cheshire.core :as json]
            [webvim.core.rope :refer [re-test]]
            [webvim.core.utils :refer [repeat-chars negzero atom?]]
            [clojure.string :as string]
            [clojure.pprint :refer [pprint]]))

(defn read-test-syntax []
  (json/parse-string (slurp "go.json") true))

;(clojure.pprint/pprint
;  (let [syntax (read-test-syntax)]
;    (re-pattern (get-in syntax [:contexts :main 0 :match]))))
(defn- compile-pattern [pattern]
  (-> pattern
      (string/replace "[:alpha:]" "\\p{Alpha}")
      (string/replace "[:alnum:]" "\\p{Alnum}")
      (string/replace "[:ascii:]" "\\p{ASCII}")
      (string/replace "[:blank:]" "\\p{Blank}")
      (string/replace "[:cntrl:]" "\\p{Cntrl}")
      (string/replace "[:digit:]" "\\d")
      (string/replace "[:graph:]" "\\p{Graph}")
      (string/replace "[:lower:]" "\\p{Lower}")
      (string/replace "[:print:]" "\\p{Print}")
      (string/replace "[:punct:]" "\\p{Punct}")
      (string/replace "[:space:]" "\\s")
      (string/replace "[:upper:]" "\\p{Upper}")
      (string/replace "[:word:]" "\\w")
      (string/replace "[:xdigit:]" "XDigit")
      re-pattern))

(defn- set-default-capture [m]
  (if (-> m :scope some?)
    (assoc-in m [:captures :0] (m :scope))
    m))

(declare compile-context)

;(defn- compile-push [{topush :push
;                      toset :set} contexts]
;  ;(println "compile-push:" topush)
;  (let [topush (or topush toset)]
;    (cond
;      (nil? topush)
;      nil
;      (vector? topush)
;      (compile-context {:matches topush} contexts)
;      :else
;      (keyword topush))))

(defn- compile-push [m]
  (if (m :push)
    (update m :push keyword)
    m))

(defn- compile-context [tocompile contexts]
  (if (tocompile :compiled?)
    tocompile
    (let [matches (tocompile :matches)]
      {:compiled? true
       :pattern (compile-pattern
                  (string/join
                    "|"
                    (filter (comp not nil?)
                            (map :match matches))))
       :meta_scope (some :meta_scope matches)
       :meta_content_scope (some :meta_content_scope matches)
       :matches (filter some?
                        (map (fn [m]
                               (let [p (if (-> m :match some?)
                                         (-> m :match compile-pattern))]
                                 ;(println "match:")
                                 ;(if (-> m :push some?)
                                 ;  (pprint m))
                                 (if (some? p)
                                   (-> m
                                       set-default-capture
                                       (assoc :match p)
                                       compile-push)))) matches))})))

(defn- expand-variable [p variables]
  (reduce-kv (fn [p k v]
               (string/replace p (str "{{" (name k) "}}") v)) p variables))

(defn test-expand-variable []
  (expand-variable "bbb{{identifier}}nainaide{{type_chars}}aaa"
                   {:identifier "identifi"
                    :type_chars "chars"}))

;(test-expand-variable)

(defn- expand-variables [contexts variables]
  ;(println "variables:" variables)
  (reduce-kv
    (fn [contexts name context]
      ;(println name)
      (assoc contexts name
             (map (fn [item]
                    (if (item :match)
                      (update item :match #(expand-variable % variables))
                      item)) context))) contexts contexts))

(defn test-expand-variables []
  (expand-variables {:main [{:match "hello{{replceme}}"}]} {:replceme "replace"}))

(defn- expand-include
  ([contexts]
    ;(pprint contexts)
    (reduce-kv
      (fn [contexts name context]
        ;(println name)
        (assoc contexts name
               (expand-include context contexts))) contexts contexts))
  ([toexpand contexts]
    ;(pprint toexpand)
    ;(println "")
    (flatten
      (filter
        some?
        (map (fn [item]
               ;(println "include:" item)
               ;(println (-> item :include keyword contexts))
               ;(println (type item))
               (if (item :include)
                 (-> item :include keyword contexts (expand-include contexts) seq)
                 item)) toexpand)))))

(def gen-id
  (let [id (atom 0)]
    #(swap! id inc)))

(defn- push-or-set [match]
  (cond
    (match :push) :push
    (match :set) :set
    :else nil))

(defn expand-anonymous-context
  ([contexts]
    (reduce-kv
      (fn [contexts name context]
        (let [[contexts context] (expand-anonymous-context contexts context)]
          (assoc contexts name context)))
      contexts contexts))
  ([contexts context]
    (reduce
      (fn [[contexts context] match]
        (let [nestkey (push-or-set match)
              push (match nestkey)]
          (if (vector? push)
            (let [nm (str "_context_" (gen-id))
                  [contexts ctx] (expand-anonymous-context contexts push)]
              [(assoc contexts (keyword nm) ctx) ;give anonymous context a unique name
               (conj context (assoc match nestkey nm))])
            [contexts (conj context match)]))) [contexts []] context)))

(defn test-expand-anonymous-context []
  (let [{contexts :contexts
         variables :variables} (json/parse-string (slurp "go.json") true)]
    ;(println variables)
    (expand-include
      (expand-variables
        (expand-anonymous-context contexts) variables))))

(defn compile-syntax [syntax]
  (update syntax :contexts
          (fn [contexts]
            (reduce-kv
              (fn [res name tocompile]
                (assoc res name
                       (compile-context {:matches tocompile} res)))
              {} (-> contexts
                     expand-anonymous-context
                     (expand-variables (syntax :variables))
                     expand-include)))))
;  (update syntax :contexts
;          (fn [contexts]
;            (-> contexts
;                expand-anonymous-context
;                expand-include)) {} contexts))

;TODO: handle contexts order
;(pprint
;  (compile-syntax (read-test-syntax)))

;(defn re-seq
;  [^java.util.regex.Pattern re s]
;  (let [m (re-matcher re s)]
;    ((fn step []
;       (when (. m (find))
;         (cons (re-groups m) (lazy-seq (step))))))))

;(defn matcher-pos [matcher]
;  (if (some? matcher)
;    (lazy-seq
;      (when (.find matcher)
;        (cons [(.start matcher) (.end matcher)]
;              (matcher-pos matcher))))))
;
(defn- conj-some
  ([a b]
    (if (some? b)
      (conj a b) a))
  ([a b & lists]
    (reduce conj-some [b] lists)))

(defn- write-outputs [context outputs s groups captures]
  ;(println "groups" groups)
  ;(println "m.captures:" (m :captures))
  ;(println "write-outputs:")
  ;(pprint (context :pattern))
  (reduce-kv (fn [outputs numkey scope]
               (let [n (-> numkey name Integer.)
                     [a b :as rg] (nth groups n)]
                 ;(println n)
                 ;(println scope)
                 ;(println (nth groups n))
                 (if (or (neg? a) (neg? b))
                   outputs
                   (conj outputs {:range rg
                                  :text (subs s a b)
                                  :scopes (conj-some [scope] (context :meta_scope))})))) outputs captures))

(defn- re-seq-pos [re pos s]
  ;(println "pos:" pos)
  (let [m (re-matcher re s)]
    (-> m
        (.region pos (count s))
        (.useTransparentBounds true)
        (.useAnchoringBounds false))
    (if (and (.find m)
             (= (.start m) pos))
      (let [n (.groupCount m)] ;"group zero is not included in this count"
        ;(println "re" re)
        ;(println "s" s)
        ;(println "pos:" pos)
        ;(println "groupCount:" n)
        (loop [i 0
               res []]
          (if (<= i n)
            (recur (inc i) (conj res [(. m (start i)) (. m (end i))]))
            (seq res)))))))

(defn test-matcher []
  (re-seq-pos #"^(;).*$\n?$" 0 ";;   Copyright 2013 Google Inc."))
  ;(first (matcher-pos (re-matcher #"(\d+)([^\d]+)" "123hello321fuck"))))


(defn- matcher-groups [m]
  (let [n (.groupCount m)]
    ;(println "groupCount:" n)
    (loop [i 0
           res []]
      (if (< i n)
        (recur (inc i)
               (conj res [(. m (start i)) (. m (end i))]))
        res))))

(defn- update-matcher [matcher p a b]
  (-> matcher
      (.usePattern p)
      (.region a b)
      (.useTransparentBounds true)
      (.useAnchoringBounds false)))

(defn- handle-pop [contexts pop?]
  (if pop?
    (pop contexts)
    contexts))

;TODO: merge meta scopes
(defn- handle-push [contexts topush all-contexts]
  (if (nil? topush)
    contexts
    (conj contexts (all-contexts (keyword topush)))))

(defn- handle-set [contexts toset all-contexts]
  (if (nil? toset)
    contexts
    (do (comment println "set:" toset)
        (conj (pop contexts) (all-contexts (keyword toset))))))

(defn- parse-line [s all-contexts contexts outputs]
  ;(println "parse-line" s)
  ;(println "parse-line" contexts)
  ;(println "parse-line" outputs)
  (loop [contexts contexts
         outputs outputs
         matcher (-> contexts peek :pattern (re-matcher s))
         last-index 0]
    ;(pprint contexts)
    ;(println last-index)
    (if (empty? contexts)
      [contexts outputs]
      (let [context (peek contexts)
            matcher (update-matcher matcher (context :pattern) last-index (count s))]
        ;(println "pattern:" (context :pattern))
        ;(println "matcher:" matcher)
        ;(println "last-index:" last-index)
        (if (.find matcher)
          (let [start (.start matcher)
                end (.end matcher)
                outputs (if (> start last-index)
                          (conj outputs {:range [last-index start]
                                         :scopes (conj-some []
                                                            (context :scope)
                                                            (context :meta_scope)
                                                            (context :meta_content_scope))
                                         :text (subs s last-index start)})
                          outputs)
                [m groups] (some (fn [m]
                                   (let [groups (re-seq-pos (m :match) start s)]
                                     ;(println "match:" (m :match))
                                     ;(println "group?" (some? groups))
                                     (if (some? groups)
                                       [m groups])))
                                 (context :matches))]
            (if (some? m)
              (let [contexts (-> contexts
                                 (handle-pop (m :pop))
                                 (handle-push (m :push) all-contexts)
                                 (handle-set (m :set) all-contexts))]
                    ;context (peek contexts)]
                ;(println "find matcher2:" start end last-index s)
                (recur contexts
                       (write-outputs context outputs s groups (m :captures))
                       matcher
                       end))
              (throw (Exception. "You just can't be here!!"))))
          [contexts (conj outputs {:range [last-index (count s)]
                                   :scopes (conj-some []
                                                      (context :scope)
                                                      (context :meta_scope)
                                                      (context :meta_content_scope))
                                   :text (subs s last-index)})])))))

(defn- parse [s all-contexts context]
  (reduce (fn [[contexts outputs] line]
            ;(println "[line]" line)
            (let [[contexts line-outputs] (parse-line line all-contexts contexts [])]
              [contexts (conj outputs line-outputs)]))
          [[context] []]
          (map #(str % \newline) (string/split-lines s))))

(defn dump-outputs [s outputs]
  (let [lines (string/split-lines s)]
    (doseq [[line output] (map list lines outputs)]
      (println line)
      (doseq [{[a b] :range
               scopes :scopes} output]
        (println (str "//"
                      (repeat-chars (negzero (- a 2)) \space)
                      (repeat-chars (if (zero? a)
                                      (- b 2)
                                      (- b a)) \^) \space (string/join \space scopes)))))))

(defn- expand-tabs [s]
  (string/join
    "\n"
    (map (fn [line]
           (webvim.core.rope/expand-tab line 0 4)) (string/split-lines s))))

(defn test-parser []
  (let [syntax (compile-syntax (read-test-syntax))
        s (expand-tabs (slurp "init.go"))
        [_ outputs] (time (parse s (syntax :contexts) (-> syntax :contexts :main)))] ;FIXME: this is wrong should keep order
    (spit
      "testoutput.go"
      (with-out-str
        (dump-outputs s (map (fn [output]
                               (map (fn [item]
                                      (update item :scopes conj-some (syntax :scope))) output)) outputs))))))

(defn test-expand-include []
  (let [contexts (:contexts (read-test-syntax))]
    (map (fn [ctx]
           (expand-include (val ctx) contexts)) contexts)))

(defn test-compile []
  (compile-syntax (read-test-syntax)))


(comment
  {:name "Lisp",
   :file_extensions ["lisp" "cl" "l" "mud" "el" "scm" "ss"],
   :scope "source.lisp",
   :contexts
   {:main
    [{:match "(;).*$\\n?",
      :scope "comment.line.semicolon.lisp",
      :captures {:1 "punctuation.definition.comment.lisp"}}
     {:match
      "(\\b(?i:(defun|defmethod|defmacro))\\b)(\\s+)((\\w|\\-|\\!|\\?)*)",
      :scope "meta.function.lisp",
      :captures
      {:2 "storage.type.function-type.lisp",
       :4 "entity.name.function.lisp"}}
     {:match "(#)(\\w|[\\\\+-=<>'\"&#])+",
      :scope "constant.character.lisp",
      :captures {:1 "punctuation.definition.constant.lisp"}}
     {:match "(\\*)(\\S*)(\\*)",
      :scope "variable.other.global.lisp",
      :captures
      {:1 "punctuation.definition.variable.lisp",
       :3 "punctuation.definition.variable.lisp"}}
     {:match "\\b(?i:case|do|let|loop|if|else|when)\\b",
      :scope "keyword.control.lisp"}
     {:match "\\b(?i:eq|neq|and|or)\\b", :scope "keyword.operator.lisp"}
     {:match "\\b(?i:null|nil)\\b", :scope "constant.language.lisp"}
     {:match
      "\\b(?i:cons|car|cdr|cond|lambda|format|setq|setf|quote|eval|append|list|listp|memberp|t|load|progn)\\b",
      :scope "support.function.lisp"}
     {:match
      "\\b((0(x|X)[0-9a-fA-F]*)|(([0-9]+\\.?[0-9]*)|(\\.[0-9]+))((e|E)(\\+|-)?[0-9]+)?)(L|l|UL|ul|u|U|F|f|ll|LL|ull|ULL)?\\b",
      :scope "constant.numeric.lisp"}
     {:match "\"",
      :captures {:0 "punctuation.definition.string.begin.lisp"},
      :push
      [{:meta_scope "string.quoted.double.lisp"}
       {:match "\"",
        :captures {:0 "punctuation.definition.string.end.lisp"},
        :pop true}
       {:match "\\\\.", :scope "constant.character.escape.lisp"}]}]}})
