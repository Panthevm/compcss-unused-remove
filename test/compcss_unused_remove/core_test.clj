(ns compcss-unused-remove.core-test
  (:require [clojure.test :refer [deftest testing is]]))

(require 'clj-ph-css.core)
(require 'compcss-unused-remove.core)
(require 'matcho.core)

(defmacro equal [a b]
  `(is (= ~a ~b)))

(defmacro defclean
  [dictionary before & after]
  `(matcho.core/match
    (->>
     (clj-ph-css.core/string->schema ~before)
     (compcss-unused-remove.core/clean ~dictionary)
     (clj-ph-css.core/schema->string))
    (apply str ~@after)))

(deftest extract-selectors-test
  (testing "keyword"
    (equal
     (compcss-unused-remove.core/file-words
      ":keyword1")
     ["keyword1"])
    (equal
     (compcss-unused-remove.core/file-words
      ":keyword1, :keyword2")
     ["keyword1" "keyword2"]))
  (testing "string"
    (equal
     (compcss-unused-remove.core/file-words
      "\"foo\"")
     ["foo"])
    (equal
     (compcss-unused-remove.core/file-words
      "\"foo\", \"bar\"")
     ["foo" "bar"]))
  (testing "symbols"
    (equal
     (compcss-unused-remove.core/file-words
      "'foo")
     ["foo"])
    (equal
     (compcss-unused-remove.core/file-words
      "'foo, 'bar")
     ["foo" "bar"]))
  (testing "hiccup-dot"
    (equal
     (compcss-unused-remove.core/file-words
      ":div.bar")
     ["div"
      "bar"])
    (equal
     (compcss-unused-remove.core/file-words
      ":div.bar.zaz")
     ["div"
      "bar"
      "zaz"])
    (equal
     (compcss-unused-remove.core/file-words
      ":div.bar.zaz-baz")
     ["div"
      "bar"
      "zaz-baz"]))
  (testing "hiccup-combinator"
    (equal
     (compcss-unused-remove.core/file-words
      ":class \"foo bar\" ")
     ["class"
      "foo"
      "bar"])))

(deftest clean-test
  (testing "types"
    (defclean #{"A"}
      "A{}
       B{}"
      "A{}"))
  (testing "classes"
    (defclean #{"A"}
      ".A{}
       .B{}"
      ".A{}"))
  (testing "identifiers"
    (defclean #{"A"}
      "#A{}
       #B{}"
      "#A{}"))
  (testing "single attribute"
    (defclean #{"A"}
      "[A]{}
       [B]{}"
      "[A]{}"))
  (testing "= attribute"
    (defclean #{"A" "W"}
      "[A=W]{}
       [A=G]{}
       [B=W]{}"
      "[A=W]{}"))
  (testing "~ attribute"
    (defclean #{"A" "C W T"}
      "[A~=W]{}
       [A~=G]{}
       [B~=W]{}"
      "[A~=W]{}"))
  (testing "^ attribute"
    (defclean #{"A" "WT"}
      "[A^=W]{}
       [A^=T]{}"
      "[A^=W]{}"))
  (testing "$ attribute"
    (defclean #{"A" "WT"}
      "[A$=T]{}
       [A$=W]{}"
      "[A$=T]{}"))
  (testing "* attribute"
    (defclean #{"A" "CWT"}
      "[A*=W]{}
       [A*=G]{}"
      "[A*=W]{}"))
  (testing "| attribute"
    (defclean #{"A" "C-S"
                "H" "C"}
      "[A|=C]{}
       [H|=C]{}
       [A|=S]{}
       [H|=S]{}"
      "[A|=C]{}"
      "[H|=C]{}"))
  (testing "not"
    (defclean {}
      ":not([A]){}"
      ":not([A]){}"))
  (testing "combinators"
    (defclean #{"A" "B"}
      "A B{}
       A>B{}
       A+B{}
       A~B{}
       A C{}
       A>C{}
       A+C{}
       A~C{}"
      "A B{}"
      "A>B{}"
      "A+B{}"
      "A~B{}")))
