(ns transnormer.model
  {:clj-kondo/config '{:linters {:unresolved-symbol    {:level :off}
                                 :unresolved-namespace {:level :off}}}}
  (:require
   [transnormer.env :as env]))

(require 'transnormer.util)

(require '[libpython-clj2.python :as py]
         '[libpython-clj2.require :refer [require-python]])

(require-python 'torch)
(py/from-import transformers AutoTokenizer GenerationConfig T5ForConditionalGeneration)

(defonce device
  (torch/device env/model-device))

(defonce tokenizer
  (delay
    (-> AutoTokenizer
        (py/py. from_pretrained "google/byt5-small"))))

(defonce transformer
  (delay
    (-> T5ForConditionalGeneration
        (py/py. from_pretrained env/model-dir)
        (py/py. to device))))

(def generation-config
  (GenerationConfig :max_new_tokens 2048
                    :early_stopping true
                    ;; ; length_penalty > 0.0 promotes longer sequences
                    :length_penalty 2.0
                    :num_beams 4))

(defn generate
  [batch]
  (let [encoded   (@tokenizer batch
                   :padding "longest"
                   :truncation false
                   :return_tensors "pt")
        input     (-> encoded (py/py.- input_ids)      (py/py. to device))
        attention (-> encoded (py/py.- attention_mask) (py/py. to device))
        generated (py/py. @transformer generate input
                          :attention_mask attention
                          :generation_config generation-config)
        decoded   (py/py. @tokenizer batch_decode generated
                          :skip_special_tokens true)]
    (py/->jvm decoded)))
