(ns transnormer.process
  (:require
   [gremid.xml :as gx]
   [hato.client :as hc]
   [taoensso.timbre :as log]
   [clojure.java.io :as io]
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

(defn sentences->xml-tok-wrap
  [sentences]
  (->> [:text (for [s sentences] [:s (for [w s] [:w {:t (:text w)}])])]
       (gx/sexp->node)
       (gx/node->events)
       (gx/write-events *out*)
       (with-out-str)))

(defn parse-xml-tok-wrap
  [sentences node]
  (for [[s s*] (map list sentences (gx/elements :s node))]
    (for [[w w*] (map list s (gx/elements :w s*))]
      (assoc w :text (gx/attr :word (gx/element :moot w*))))))

(defn read-xml
  [input]
  (with-open [input (io/input-stream input)]
    (-> input gx/read-events gx/events->node)))

(def url
  "https://www.deutschestextarchiv.de/public/cab/query")

(defn cab-analyze
  [sentences]
  (when (seq sentences)
    (let [xml (sentences->xml-tok-wrap sentences)
          req {:url         url
               :method      :post
               :as          :stream
               :form-params {"fmt" "ftwxml"
                             "qd"  xml}}]
      (->> (hc/request req) :body read-xml (parse-xml-tok-wrap sentences)))))

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
        cab-result (cab-analyze tokenized)
        result     (tokenize (first (model/generate [s])))
        result     [(into [] sentences->texts-xf tokenized)
                    (into [] sentences->texts-xf result)
                    (into [] sentences->texts-xf cab-result)]]
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
