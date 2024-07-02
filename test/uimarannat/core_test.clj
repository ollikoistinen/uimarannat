(ns uimarannat.core-test
  (:require [clojure.test :refer :all]
            [uimarannat.core :refer :all]
            [cheshire.core]
            [org.httpkit.client :as http]))

(defn generate-timestamp-now-minus-hours [n]
  (-> (java.time.ZonedDateTime/now)
      (.minusHours n)
      .toOffsetDateTime
      .toString))

(def sompasauna-key :ffffsompasauna)
(def sompasauna-response
  (let [p (promise)]
    (deliver p {:status 200
                :body (str "{
 \"meta\": {
  \"name\": \"Sompasauna\",
  \"location\": \"Hermanninranta\",
  \"district\": \"Hermanni\",
  \"lat\": 60.194102,
  \"lon\": 24.984992,
  \"servicemap_url\": \"https://palvelukartta.hel.fi/fi/unit/54929\",
  \"site_url\": \"https://www.sompasauna.fi/\",
  \"site_title\": \"Sompasauna Ry\",
  \"info\": \"Alhaisen vedenkorkeuden johdosta mittari voi mitata v\u00e4lill\u00e4 ilman l\u00e4mp\u00f6tilaa.\",
  \"links\": {},
  \"valid_from\": \"2022-05-05T16:00:00Z\",
  \"properties\": {
   \"installation depth\": -0.39,
   \"installation platform\": \"steps\",
   \"installation method\": \"static\"
  },
  \"file_created\": \"2024-06-26T18:20:03.333337+03:00\"
 },
 \"data\": [
  {
   \"time\": \"" (generate-timestamp-now-minus-hours 1) "\",
   \"temp_air\": 22.14,
   \"temp_water\": 30.5
  },
  {
   \"time\": \"" (generate-timestamp-now-minus-hours 2)"\",
   \"temp_air\": 19.5,
   \"temp_water\": 18.93
  },
  {
   \"time\": \"" (generate-timestamp-now-minus-hours 3)"\",
   \"temp_air\": 18.45,
   \"temp_water\": 78.68
  },
  {
   \"time\": \"" (generate-timestamp-now-minus-hours 4)"\",
   \"temp_air\": 17.48,
   \"temp_water\": 18.87
  }]}}")})))

(def kuusijarvi-key :ffffkuusijarvi)
(def kuusijarvi-response
  (let [p (promise)]
    (deliver p {:status 200
                :body (str "{
 \"meta\": {
  \"name\": \"Kuusij\u00e4rvi\",
  \"location\": \"Uimaranta\",
  \"district\": \"Vantaa (Kuninkaanm\u00e4ki)\",
  \"lat\": 60.313343,
  \"lon\": 25.113967,
  \"servicemap_url\": \"https://palvelukartta.hel.fi/fi/unit/42611\",
  \"site_url\": \"https://www.vantaa.fi/vapaa-aika/luonto_ja_ulkoilu/retkeily/kuusijarven_ulkoilualue\",
  \"fieldmap\": {
   \"temp_water\": \"temp_out1\",
   \"air_rh\": \"temprh_rh\",
   \"air_temp\": \"temprh_temp\"
  },
  \"properties\": {
   \"installation depth\": -0.3,
   \"installation platform\": \"steps\",
   \"installation method\": \"static\"
  },
  \"file_created\": \"2024-06-27T15:20:04.016628+03:00\"
 },
 \"data\": [
  {
   \"time\": \"" (generate-timestamp-now-minus-hours 1) "\",
   \"temp_air\": 22.14,
   \"temp_water\": 19.5
  },
  {
   \"time\": \"" (generate-timestamp-now-minus-hours 2)"\",
   \"temp_air\": 19.5,
   \"temp_water\": 18.93
  },
  {
   \"time\": \"" (generate-timestamp-now-minus-hours 3)"\",
   \"temp_air\": 18.45,
   \"temp_water\": 18.68
  },
  {
   \"time\": \"" (generate-timestamp-now-minus-hours 4)"\",
   \"temp_air\": 17.48,
   \"temp_water\": 98.87
  }]}}")})))

(def index-response
  (let [p (promise)]
    (deliver p {:status 200
                :body (str "{\"" (name sompasauna-key) "\": {}, \"" (name kuusijarvi-key) "\": {}}")})))

(def index-url "https://iot.fvh.fi/opendata/uiras/uiras-meta.json")

(defn mock-get
  [url]
  (let [spot-url (fn [spot-key]
                   (let [base-url "https://iot.fvh.fi/opendata/uiras/"]
                     (str base-url (name spot-key) "_v1.json")))]
    (cond
      (= url (spot-url sompasauna-key)) sompasauna-response
      (= url (spot-url kuusijarvi-key)) kuusijarvi-response
      (= url index-url)                 index-response
      :else                             (throw (Exception. "Unknown url requested")))))

(deftest datetime-is-within-last-hours-success
  (let [datetime (java.time.ZonedDateTime/now)]
    (is (datetime-is-within-last-hours? datetime 3))))

(deftest datetime-is-within-last-hours-fail
  (let [datetime (-> (java.time.ZonedDateTime/now)
                     (.minusHours 4))]
    (is (false? (datetime-is-within-last-hours? datetime 3)))))

(deftest get-latest-measurement-returns-correct-measurement
  (let [data   (-> sompasauna-response
                   deref
                   :body
                   (cheshire.core/parse-string true)
                   :data)
        latest (get-latest-measurement data)]
    (is (= (:temp_water latest) 30.5))))

(deftest best-swimming-spot-returns-correct-result
  (with-redefs [http/get mock-get
                println  (fn [s])]
    (let [measurement-expiration-hours 3
          swimming-spot-keys           (get-swimming-spot-keys)
          swimming-spot-details        (fetch-swimming-spots-details swimming-spot-keys)
          best-spot                    (get-best-swimming-spot swimming-spot-details measurement-expiration-hours)
          best-spot-latest-measurement (get-latest-measurement (:data best-spot))]
      (is (= 30.5 (:temp_water best-spot-latest-measurement))))))
