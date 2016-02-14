(ns webvim.lang.go
  (:use webvim.core.lang
        webvim.core.event
        webvim.core.rope
        webvim.core.line
        webvim.core.utils
        clojure.pprint
        webvim.indent))

(println "load go language")

(defmethod init-file-type ".go"
  [buf]
  (-> buf
      (assoc-in [:language :id] ::go)
      (assoc-in [:language :name] "Go")
      (assoc :tabsize 4)
      (assoc :expandtab false)))

(defmethod indent-pos ::go
  [lang r pos]
  (clang-indent r pos))

(defmethod indent-trigger? ::go
  [lang keycode]
  (= keycode "}"))

(defn- parse-hunks[diff]
  (re-seq #"(?<=^|\R)@@ -(\d+),?(\d+)? \+(\d+),?(\d+)? @@.*?\R?([\s\S]*?)(?=$|(?<=\R)@@.*?@@)" diff))

(defn- parse-hunk[from lines]
  (loop [[line & lines] lines
         linenum (dec from) ;start from 0
         changes nil
         change nil ;by line, NOT by position
         lasttype " "]
    ;(println "parse-hunk")
    ;(print "line:" line)
;    (pprint lines)
;    (println linenum)
;    (println changes)
;    (println "change:" change)
;    (println (str "lasttype:[" lasttype "]"))
    (if (nil? line)
      (if (nil? change) changes (conj changes change))
      (let [type (str (first line))
            ;_ (println (str "type:[" type "]"))
            [linenum changes change]
            (cond
              (and (= lasttype " ") (= type " "))
              [(inc linenum) changes change]
              (and (= lasttype "-") (= type "-"))
              [(inc linenum) changes (update-in change [:len] inc)]
              (and (= lasttype "+") (= type "+"))
              [linenum changes (update-in change [:to] str (subs line 1))]
              (and (= lasttype " ") (= type "+"))
              [linenum changes {:from linenum :len 0 :to (subs line 1)}]
              (and (= lasttype " ") (= type "-"))
              [(inc linenum) changes {:from linenum :len 1 :to ""}]
              (and (= lasttype "-") (= type "+"))
              [linenum changes (assoc change :to (subs line 1))]
              (and (= lasttype "-") (= type " "))
              [(inc linenum) (conj changes change) nil]
              (and (= lasttype "+") (= type "-"))
              [(inc linenum) changes (update-in change :len inc)]
              (and (= lasttype "+") (= type " "))
              [(inc linenum) (conj changes change) nil]
              :else
              (throw (Exception. "parse diff error")))]
        (recur lines linenum changes change type)))))

;http://www.gnu.org/software/diffutils/manual/diffutils.html#Unified-Format
(defn- parse-diff[diff]
  (println "parse-diff")
  ;(println diff)
  ;(pprint (parse-hunks diff))
  (loop [[hunk & hunks] (parse-hunks diff)
         changes nil]
    ;(println "hunk:" hunk)
    (if (nil? hunk)
      changes
      (let [from (parse-int (second hunk)) ;from line number
            content (last hunk) ;hunk content
            lines (map #(str % "\n") (clojure.string/split-lines content))]
        (recur hunks
               (concat (parse-hunk from lines) changes))))))

(comment
  (defn test-parse-diff[]
    (pprint (parse-diff
"diff <standard input> gofmt/<standard input>
--- lao  2002-02-21 23:30:39.942229878 -0800
+++ tzu  2002-02-21 23:30:50.442260588 -0800
@@ -1,7 +1,6 @@
-The Way that can be told of is not the eternal Way;
-The name that can be named is not the eternal name.
 The Nameless is the origin of Heaven and Earth;
-The Named is the mother of all things.
+The named is the mother of all things.
+
 Therefore let there always be non-being,
   so we may see their subtlety,
 And let there always be being,
@@ -9,3 +8,6 @@
 The two are the same,
 But after they are produced,
   they have different names.
+They both may be called deep and profound.
+Deeper and more profound,
+The door of all subtleties!\n"))))

(defn- apply-changes[buf changes]
  (let [lines (pos-lines-seq+ (buf :str))]
    (pprint changes)
    (reduce (fn[buf {from :from len :len to :to}]
              (println (nth lines from))
              (let [[a _] (nth lines from)
                    [b _] (nth lines (+ from len))]
                (buf-replace buf a b to))) buf changes)))

(defn- format-buffer[buf]
  (if (-> buf :language :id (= ::go))
    (let [res (clojure.java.shell/sh "goimports" "-d" :in (str (buf :str)))]
      (println "gofmt")
      (if (-> res :exit zero? not)
        (assoc buf :message (res :err))
        (-> buf
            (apply-changes (parse-diff (str (res :out))))
            save-undo)))
    buf))

(defonce ^:private listener
  (listen :write-buffer
        (fn[buf]
          (format-buffer buf))))
