(ns webvim.ui.lib.keycode)

(defn char32bits? [ch]
  (let [code (.charCodeAt ch 0)]
    (cond
      (<= 0xDFFF code 0xD800)
      true
      (> (count ch) 1)
      (let [code (.charCodeAt ch 1)]
        (<= 0xFE00 code 0xFE0F))
      :else
      false)))

(defn- special-key? [ch]
  (cond
    (<= (count ch) 1)
    false
    (= (count ch) 2)
    (not (char32bits? ch))
    :else
    true))

(def KEYCODE-KEYDOWN
  {8 "bs"
   9 "tab"
   12 "num"
   13 "cr"
   19 "pause"
   20 "caps"
   27 "esc"
   32 "space"
   33 "pageup"
   34 "pagedown"
   35 "end"
   36 "home"
   37 "left"
   38 "up"
   39 "right"
   40 "down"
   44 "print"
   45 "insert"
   46 "delete"
   106 "num_multiply"
   107 "num_add"
   108 "num_enter"
   109 "num_subtract"
   110 "num_decimal"
   111 "num_divide"
   112 "f1"
   113 "f2"
   114 "f3"
   115 "f4"
   116 "f5"
   117 "f6"
   118 "f7"
   119 "f8"
   120 "f9"
   121 "f10"
   122 "f11"
   123 "f12"
   124 "print"
   144 "num"
   145 "scroll"})

(def KEYCODE-DIC
  {8 "bs"
   9 "tab"
   12 "num"
   13 "cr"
   16 "shift"
   17 "ctrl"
   18 "alt"
   19 "pause"
   20 "caps"
   27 "esc"
   32 "space"
   33 "pageup"
   34 "pagedown"
   35 "end"
   36 "home"
   37 "left"
   38 "up"
   39 "right"
   40 "down"
   44 "print"
   45 "insert"
   46 "delete"
   48 "0"
   49 "1"
   50 "2"
   51 "3"
   52 "4"
   53 "5"
   54 "6"
   55 "7"
   56 "8"
   57 "9"
   65 "a"
   66 "b"
   67 "c"
   68 "d"
   69 "e"
   70 "f"
   71 "g"
   72 "h"
   73 "i"
   74 "j"
   75 "k"
   76 "l"
   77 "m"
   78 "n"
   79 "o"
   80 "p"
   81 "q"
   82 "r"
   83 "s"
   84 "t"
   85 "u"
   86 "v"
   87 "w"
   88 "x"
   89 "y"
   90 "z"
   91 "cmd"
   92 "cmd"
   93 "cmd"
   96 "num_0"
   97 "num_1"
   98 "num_2"
   99 "num_3"
   100 "num_4"
   101 "num_5"
   102 "num_6"
   103 "num_7"
   104 "num_8"
   105 "num_9"
   106 "num_multiply"
   107 "num_add"
   108 "num_enter"
   109 "num_subtract"
   110 "num_decimal"
   111 "num_divide"
   112 "f1"
   113 "f2"
   114 "f3"
   115 "f4"
   116 "f5"
   117 "f6"
   118 "f7"
   119 "f8"
   120 "f9"
   121 "f10"
   122 "f11"
   123 "f12"
   124 "print"
   144 "num"
   145 "scroll"
   186 ";"
   187 "="
   188 ","
   189 "-"
   190 "."
   191 "/"
   192 "`"
   219 "["
   220 "\\"
   221 "]"
   222 "'"
   223 "`"
   224 "cmd"
   225 "alt"
   57392 "ctrl"
   63289 "num"
   59 ";"
   61 "="
   173 "-"})

(defn- map-shift-char [s]
  (or
    ({"s-a" "A"
      "s-b" "B"
      "s-c" "C"
      "s-d" "D"
      "s-e" "E"
      "s-f" "F"
      "s-g" "G"
      "s-h" "H"
      "s-i" "I"
      "s-j" "J"
      "s-k" "K"
      "s-l" "L"
      "s-m" "M"
      "s-n" "N"
      "s-o" "O"
      "s-p" "P"
      "s-q" "Q"
      "s-r" "R"
      "s-s" "S"
      "s-t" "T"
      "s-u" "U"
      "s-v" "V"
      "s-w" "W"
      "s-x" "X"
      "s-y" "Y"
      "s-z" "Z"
      "s-1" "!"
      "s-2" "@"
      "s-3" "#"
      "s-4" "$"
      "s-5" "%"
      "s-6" "^"
      "s-7" "&"
      "s-8" "*"
      "s-9" "("
      "s-0" ")"
      "s--" "_"
      "s-=" "+"
      "s-[" "{"
      "s-]" "}"
      "s-;" ":"
      "s-'" "\""
      "s-," "<"
      "s-." ">"
      "s-`" "~"
      "s-\\" "|"
      "s-/" "?"} s) s))



