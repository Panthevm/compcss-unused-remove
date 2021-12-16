(ns compcss-unused-remove.core)

(require 'clojure.java.io)
(require 'clojure.string)

(def words-pattern #"([a-zA-Z0-9\*_-]+)")

(defn directory-files
  [directory-path]
  (when (.exists (clojure.java.io/as-file directory-path))
    (->> (clojure.java.io/file directory-path)
         (file-seq)
         (filter (memfn isFile)))))

(defn file-words
  [content]
  (map second (re-seq words-pattern content)))

(defn directories-dictionary
  [paths]
  (->>
   (mapcat directory-files paths)
   (mapcat (comp file-words slurp))
   (set)))

(defn attribute?
  [dictionary member]
  (some
   (fn [value]
     (when (contains? dictionary (:name member))
       (let [attribute (:attribute member)]
         (case (-> member :operator :name)
           "="  (= value (:attribute member))
           "^=" (clojure.string/starts-with? value attribute)
           "$=" (clojure.string/ends-with?   value attribute)
           "*=" (clojure.string/includes?    value attribute)
           "~=" (some #(= attribute %)
                      (clojure.string/split value #" "))
           "|=" (or (= value attribute)
                    (clojure.string/starts-with? value (str attribute "-")))
           true))))
   dictionary))

(defn member?
  [dictionary member]
  (case (:type member)
    :member-simple
    (case (:group member)
      :type
      (contains? dictionary (:value member))
      :class
      (contains? dictionary (subs (:value member) 1))
      :identifier
      (contains? dictionary (subs (:value member) 1))
      true)
    :selector-attribute
    (attribute? dictionary member)
    true))

(defn clean
  [dictionary stylesheets]
  (seq
   (keep
    (fn [stylesheet]
      (case (:type stylesheet)
        :selector
        (when (every? #(member? dictionary %)
                      (:members stylesheet))
          stylesheet)
        :style-rule
        (let [new-selectors (clean dictionary (:selectors stylesheet))]
          (when (seq new-selectors)
            (assoc stylesheet :selectors new-selectors)))
        :media-rule
        (let [new-rules (clean dictionary (:rules stylesheet))]
          (when (seq new-rules)
            (assoc stylesheet :rules new-rules)))
        stylesheet))
    stylesheets)))

(defn middleware
  [handler]
  (fn [configuration db]
    (handler
     configuration
     (update db :compcss.core/output-stylesheets
             (fn [stylesheets]
               (-> (get-in configuration [:input :clj])
                   (directories-dictionary)
                   (clean stylesheets)))))))