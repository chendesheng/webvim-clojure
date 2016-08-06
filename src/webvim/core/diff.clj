(ns webvim.core.diff
  (:require [clj-diff.core :as d]
            [clojure.string :as string])
  (:use webvim.core.utils
        webvim.core.line
        webvim.core.pos
        clojure.pprint
        webvim.core.rope))

(defn- parse-hunks [diff]
  (re-seq #"@@ -(\d+),?(\d+)? \+(\d+),?(\d+)? @@([\s\S]*?)(?=$|(?<=\r?\n)@@.*?@@)" diff))

(defn- parse-hunk [from lines]
;  (println "parse-hunk")
;  (println (dec from))
;  (pprint lines)
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
    ;(println "change:" change)
    ;(println (str "lasttype:[" lasttype "]"))
    (if (nil? line)
      (if (nil? change) changes (conj changes change))
      (let [tmp (str (first line))
            type (if (contains? #{" " "-" "+"} tmp) tmp " ")
            ;_ (println (str "type:[" type "]"))
            ;_ (println linenum)
            ;_ (pprint change)
            [linenum changes change]
            (cond
              (and (= lasttype " ") (= type " "))
              [(inc linenum) changes change]
              (and (= lasttype "-") (= type "-"))
              [(inc linenum) changes (update change :len inc)]
              (and (= lasttype "+") (= type "+"))
              [linenum changes (update change :to str (subs line 1))]
              (and (= lasttype " ") (= type "+"))
              [linenum changes {:from linenum :len 0 :to (subs line 1)}]
              (and (= lasttype " ") (= type "-"))
              [(inc linenum) changes {:from linenum :len 1 :to ""}]
              (and (= lasttype "-") (= type "+"))
              [linenum changes (assoc change :to (subs line 1))]
              (and (= lasttype "-") (= type " "))
              [(inc linenum) (conj changes change) nil]
              (and (= lasttype "+") (= type "-"))
              [(inc linenum) changes (update change :len inc)]
              (and (= lasttype "+") (= type " "))
              [(inc linenum) (conj changes change) nil])]
        ;(pprint changes)
        (recur lines linenum changes change type)))))

;http://www.gnu.org/software/diffutils/manual/diffutils.html#Unified-Format
(defn parse-diff [diff]
  (println "parse-diff")
  ;(print diff)
  ;(pprint (parse-hunks diff))
  (loop [[hunk & hunks] (parse-hunks diff)
         changes nil]
    ;(println "hunk:" hunk)
    (if (nil? hunk)
      changes
      (let [from (parse-int (second hunk)) ;from line number
            content (last hunk)
            lines (split-lines content)]
        (recur hunks
               ;start from start next line
               ;@@ -2,5 +2,5 @@ var viewport = {};
               ; var lineHeight = 21;
               ; 
               ; function wrapActiveId(keys) {
               ;- //minimize overhead of add id to every input keys
               ;+  //minimize overhead of add id to every input keys
               ; }
               (concat (parse-hunk from (next lines)) changes))))))

(comment
  (defn test-parse-diff []
    (pprint (parse-diff
              "diff <standard input> gofmt/<standard input>
--- lao  2002-02-21 23:30:39.942229878 -0800
+++ tzu  2002-02-21 23:30:50.442260588 -0800
@@ -1,7 +1,6 @@
 first line
+insert line"))
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

;(defn- pprint2[buf]
;  (println "buffer1:")
;  (pprint (buf :str))
;  buf)

(defn- fix-position [buf]
  (let [r (buf :str)
        buf (if (empty? r) (buf-insert buf 0 "\n") buf)]
    (if (>= (buf :pos) (-> buf :str count))
      (buf-end buf)
      buf)))

(defn apply-line-changes [buf changes]
  ;(pprint changes)
  ;(pprint (count (buf :str)))
  ;(pprint (count lines))
  ;(pprint lines)
  ;(pprint changes)
  (fix-position
    (reduce
      (fn [buf {from :from len :len to :to}]
          ;(println "line:" from (nth lines from))
          ;(pprint  (str (subr r (nth lines from))))
          ;(println (nth lines (+ from len) [len 0]))
          ;(println "line:" (+ from len) (nth lines (+ from len)) (str (subr r (nth lines (+ from len)))))
        (let [to (ensure-ends-with-newline to) ;some format tool may removes last \newline
              [a _] (line-range buf from)
              [_ b] (line-range buf (dec (+ from len)))]
          ;(println "a b" a b)
          ;(print "to" to)
          (buf-replace buf a b to))) buf changes)))

;diff tool output unified format
(defn diff-tool [s1 s2]
  (with-temp-file
    s1 (fn [f]
         (with-temp-file
           s2
           (partial clojure.java.shell/sh "diff" "-u" "-Z" f)))))

(defn- filelines [f]
  (clojure.string/split-lines (slurp f)))

(defn- count-changes [changes]
  (+ (-> changes :+ count)
     (-> changes :- count)))

(defn- merge-changes [changes]
  (map (fn [c]
         (case (count c)
           1 (first c)
           2 (let [[[_ line] [_ _ s]] c]
               [:-+ line s])))
       (partition-by second
                     changes)))

;About how to sort-by in reverse order, one might think why not just using (reverse (sort-by ...))?
;This is because reverse will make the sort action unstable.
(def ^:private reverse-compare #(compare %2 %1))

(defn diff [s1 s2]
  (let [{additions :+ deletions :-} (d/diff
                                      (string/split-lines s1)
                                      (string/split-lines s2))]
    (merge-changes
      (sort-by second
               reverse-compare
               (concat
                 (map (fn [de]
                        [:- de]) deletions)
                 (map (fn [[line & ss]]
                        [:+ (inc line) (string/join (map #(str % \newline) ss))]) additions))))))

(comment
  (let [s1 "1\n2\n3"
        s2 "1\n3\n3"]
    (vec (diff s1 s2)))

  (let [s1 "1\n2\n3"
        s2 "1\n3\n3"
        changes (diff s1 s2)
        buf (patch (webvim.core.buffer/create-buf "" "" s1) changes)]
    (println changes)
    (pprint (-> buf :str str))
    (pprint s2)
    (= (-> buf :str str) s2)))

(defn- patch-deletions [buf deletions]
  (reduce
    (fn [buf line]
      (buf-delete buf (line-range buf line))) buf deletions))

(defn- patch-additions [buf additions]
  (reduce
    (fn [buf [line & ss]]
      (buf-insert buf (first (line-range buf line)) (string/join ss))) buf additions))

(defn patch [buf changes]
  (fix-position
    (reduce
      (fn [buf [type line s]]
        (case type
          :-+ (buf-replace buf (line-range buf line) s)
          :- (buf-delete buf (line-range buf line))
          :+ (buf-insert buf (first (line-range buf line)) s))) buf changes)))

;(d/diff (filelines "c:\\users\\roy\\desktop\\hosts") (filelines "c:\\users\\roy\\desktop\\hosts2"))
