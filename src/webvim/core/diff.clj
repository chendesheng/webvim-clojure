(ns webvim.core.diff
  (:use webvim.core.utils
        webvim.core.line
        webvim.core.pos
        clojure.pprint
        webvim.core.rope))

(defn- parse-hunks[diff]
  (re-seq #"@@ -(\d+),?(\d+)? \+(\d+),?(\d+)? @@([\s\S]*?)(?=$|(?<=\R)@@.*?@@)" diff))

(defn- parse-hunk[from lines]
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
              [(inc linenum) (conj changes change) nil])]
        ;(pprint changes)
        (recur lines linenum changes change type)))))

;http://www.gnu.org/software/diffutils/manual/diffutils.html#Unified-Format
(defn parse-diff[diff]
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
            lines (map #(str % "\n") (clojure.string/split-lines content))]
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
  (defn test-parse-diff[]
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

(defn- fix-position[buf]
  (let [r (buf :str)
        buf (if (empty? r) (buf-insert buf 0 "\n") buf)]
    (if (>= (buf :pos) (-> buf :str count))
      (buf-end buf)
      buf)))

(defn apply-line-changes[buf changes]
  ;(pprint changes)
  (let [lines (pos-lines-seq+ (buf :str))
        r (buf :str)
        cnt (count r)]
;    (pprint (count (buf :str)))
;    (pprint (count lines))
;    (pprint lines)
;    (pprint changes)
    (fix-position
      (reduce
        (fn[buf {from :from len :len to :to}]
          ;(println "line:" from (nth lines from))
          ;(pprint  (str (subr r (nth lines from))))
          ;(println (nth lines (+ from len) [len 0]))
          ;(println "line:" (+ from len) (nth lines (+ from len)) (str (subr r (nth lines (+ from len)))))
          (let [[a _] (nth lines from [cnt 0])
                [b _] (nth lines (+ from len) [cnt 0])]
            ;(println "a b" a b)
            ;(print "to" to)
            (buf-replace buf a b to))) buf changes))))

