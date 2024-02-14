(ns transnormer.process
  (:require
   [transnormer.model :as model]
   [transnormer.collate :as collate]
   [clojure.string :as str])
  (:import
   (de.ids_mannheim.korap.tokenizer DerekoDfaTokenizer_de)
   (opennlp.tools.util Span)))

(def tokenizer
  (DerekoDfaTokenizer_de.))

(defn span->map
  ([s span]
   (span->map 0 s span))
  ([offset s ^Span span]
   (let [start (+ offset (.getStart span))
         end   (+ offset (.getEnd span))]
     {:text  (subs s start end)
      :start start
      :end   end})))

(defn segment-sentences
  [s]
  (into [] (map (partial span->map s)) (.sentPosDetect tokenizer s)))

(defn segment-tokens
  [offset text sentence]
  (map (partial span->map offset text) (.tokenizePos tokenizer sentence)))

(defn assoc-space-after**
  [[{:keys [end] :as token} next-token]]
  (assoc token :space-after? (< end (get next-token :start end))))

(defn assoc-space-after*
  [[tokens next-tokens]]
  (let [tokens      (concat tokens (take 1 next-tokens))
        token-pairs (partition-all 2 1 tokens)
        tokens      (map assoc-space-after** token-pairs)
        tokens      (cond-> tokens (seq next-tokens) (butlast))]
    (vec tokens)))

(defn assoc-space-after
  [sentences]
  (let [sentence-pairs (partition-all 2 1 sentences)]
    (map assoc-space-after* sentence-pairs)))

(defn tokenize
  [s]
  (let [sentences (segment-sentences s)]
    (->>
     (for [{s-text :text s-start :start} sentences]
       (segment-tokens s-start s s-text))
     (assoc-space-after)
     (vec))))

(def sentences->texts-xf
  (comp (mapcat identity)
        (map (fn [{:keys [text space-after?]}]
               (str text (when space-after? " "))))))

(def match?
  (partial every? some?))

(defn alignment->strs
  [alignment]
  (into [] (map str/join) (collate/transpose alignment)))

(defn align-normalizations
  [s]
  (let [tokenized  (tokenize s)
        result     (tokenize (first (model/generate [s])))
        result     [(into [] sentences->texts-xf tokenized)
                    (into [] sentences->texts-xf result)]]
    (->> (collate/align-tokens result)
         (partition-by match?)
         (map alignment->strs)
         (vec))))

(comment
  (->>
   (str "Bey dieser Gelegenheit macht er nun von den alten Germaniern folgende "
        "Beschreibung, die wir unsern Lesern zu Gefallen, in einer getreuen "
        "Übersetzung grossen Theils abschreiben wollen.")
   (align-normalizations)))
