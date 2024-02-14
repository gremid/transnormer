(ns transnormer.html
  {:clj-kondo/config '{:linters {:unresolved-symbol {:level :off}}}}
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   [lambdaisland.ornament :as o :refer [defstyled]]
   [transnormer.env :as env]
   [transnormer.url :as url]))

(def cp
  (str env/http-context-path "/"))

(def title
  "Transnormer – A lexical normalizer for historical spelling variants (German)")

(defstyled header :header
  :relative
  [:nav
   :flex :items-center :justify-start :w-full :p-4 :space-x-8
   [:#menu
    [:a :hover:underline
     [:.title :text-2xl]]]
   [:#logo
    [:a :flex
     [:span :sr-only]
     [:img :h-24 :w-auto]]]]
  ([]
   [:nav
    [:div#logo
     [:a {:href cp}
      [:span "Transnormer"]
      [:img {:src (str cp "assets/bbaw_logo.svg")}]]]
    [:div#menu
     [:a {:href cp :title title}
      [:span.title title]]]]))

(defstyled footer :footer
  [:p :py-8 :text-center :text-base :text-gray-500]
  ([]
   [:<>
    [:p
     [:strong "Nutzungsbedingungen: "]
     "Alle Rechte vorbehalten. Nur für die projektinterne Nutzung."]]))

(defstyled page :html
  :h-full :text-ul-basalt :font-sans
  [:body :h-full :font-sans]
  ([page-title & contents]
   [:<> {:lang "de"}
    [:head
     [:meta {:charset "UTF-8"}]
     [:meta {:name "viewport", :content "width=device-width, initial-scale=1.0"}]
     [:link {:rel "stylesheet" :href (str cp "assets/styles.css")}]
     [:title (str page-title " – " title)]]
    [:body
     [header]
     [:main contents]
     [footer]]]))

(def sample-input
  (str "Bey dieser Gelegenheit macht er nun von den alten Germaniern folgende "
       "Beschreibung, die wir unsern Lesern zu Gefallen, in einer getreuen "
       "Übersetzung grossen Theils abschreiben wollen."))

(defstyled query-form :form
  :bg-slate-200 :w-full :p-4
  [:.controls :max-w-5xl :mx-auto :px-4 :sm:px-6 :lg:px-8
   [:label :block :text-sm :font-medium :leading-6 :text-slate-900]
   [:textarea :block :w-full :mt-2 :p-2
    :text-slate-900 :placeholder:text-slate-400]
   [:button :mt-2 :p-2 :font-semibold :text-white :bg-red-700]]
  ([req q]
   [:<> {:method "post" :action (url/index req)}
    [:div.controls
     [:label {:for "q"} "Input:"]
     [:textarea {:name        "q"
                 :id          "q"
                 :placeholder "Text to normalize…"
                 :rows        "4"
                 :onfocus     "this.select();"
                 :autofocus   ""} (or q sample-input)]
     [:button {:type "submit"} "Normalize"]]]))

(defstyled query-results :section
  :mt-6 :w-full :p-8
  [:table :w-full :max-w-5xl :mx-auto :px-4 :sm:px-6 :lg:px-8 :border-1 :border-slate-200
   [:tr.match :bg-lime-300]
   [:tr.normalized :bg-amber-300]
   [:th :p-2 :bg-slate-200]
   [:td :p-2]]
  ([alignment]
   (when alignment
     [:table
      [:tr
       [:th "Input"]
       [:th "Transnormer"]]
      (for [[input transnormer] alignment]
        [:tr {:class (cond
                       (= input transnormer) "identical"
                       :else                 "normalized")}
         [:td input]
         [:td transnormer]])])))

(defstyled request-dump :pre
  :mt-6 :p-4 :text-sm :text-gray-500 :border-1 :border-gray-200
  ([req]
   [:<> (with-out-str (pprint req))]))

(defn index
  [req q alignment]
  [page "Normalize Samples"
   [query-form req q]
   [query-results alignment]
   #_[request-dump req]])

(o/set-tokens! {:tw-version 3
                :colors     {:ul-basalt     "262a31"
                             :ul-garnet     "b02f2c"
                             :ul-carnelian  "d8413e"
                             :ul-aquamarine "8ac2d1"}})

(try
  (let [css (o/defined-styles {:preflight? true})]
    (spit (doto (io/file "src" "public" "styles.css") (io/make-parents)) css)
    (spit (doto (io/file "classes" "public" "styles.css") (io/make-parents)) css))
  (catch java.io.FileNotFoundException _))
