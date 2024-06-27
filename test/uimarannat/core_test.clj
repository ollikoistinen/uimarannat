(ns uimarannat.core-test
  (:require [clojure.test :refer :all]
            [uimarannat.core :refer :all]
            [cheshire.core]))


(defn generate-timestamp-now-minus-hours [n]
  (-> (java.time.ZonedDateTime/now)
      (.minusHours n)
      .toOffsetDateTime
      .toString))

(def sompasauna-key :ffffsompasauna)
(def sompasauna-response
  (cheshire.core/parse-string
    (str "{\"body\":{
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
   \"time\": \""(generate-timestamp-now-minus-hours 1) "\",
   \"temp_air\": 22.14,
   \"temp_water\": 30.5
  },
  {
   \"time\": \""(generate-timestamp-now-minus-hours 2)"\",
   \"temp_air\": 19.5,
   \"temp_water\": 18.93
  },
  {
   \"time\": \""(generate-timestamp-now-minus-hours 3)"\",
   \"temp_air\": 18.45,
   \"temp_water\": 18.68
  },
  {
   \"time\": \""(generate-timestamp-now-minus-hours 4)"\",
   \"temp_air\": 17.48,
   \"temp_water\": 18.87
  }]}}") true))

(def kuusijarvi-key :ffffkuusijarvi)
(def kuusijarvi-response
  (cheshire.core/parse-string
    (str "{\"body\":{
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
   \"time\": \""(generate-timestamp-now-minus-hours 1) "\",
   \"temp_air\": 22.14,
   \"temp_water\": 19.5
  },
  {
   \"time\": \""(generate-timestamp-now-minus-hours 2)"\",
   \"temp_air\": 19.5,
   \"temp_water\": 18.93
  },
  {
   \"time\": \""(generate-timestamp-now-minus-hours 3)"\",
   \"temp_air\": 18.45,
   \"temp_water\": 18.68
  },
  {
   \"time\": \""(generate-timestamp-now-minus-hours 4)"\",
   \"temp_air\": 17.48,
   \"temp_water\": 18.87
  }]}}") true))

(def index-response {sompasauna-key {}
                     kuusijarvi-key {}})

(def index-url "https://iot.fvh.fi/opendata/uiras/uiras-meta.json")
(defn spot-url [spot-key]
  (let [base-url "https://iot.fvh.fi/opendata/uiras/"]
    (str base-url (name key) "_v1.json")))

(defn mock-get
  ([url req respond raise]
   (let [sompasauna-url (spot-url sompasauna-key)
         kuusijarvi-url (spot-url kuusijarvi-key)]
     (case url
       sompasauna-url (respond sompasauna-response)
       kuusijarvi-url (respond kuusijarvi-response)
       (throw (Exception. "Unknown url requested")))))
  ([url req]
   (if (= url index-url)
     index-response
     (throw (Exception. "Unknown url requested")))))

(defn mock-println [s]
  )

(deftest datetime-is-within-last-hours-success
  (let [datetime (java.time.ZonedDateTime/now)]
    (is (datetime-is-within-last-hours? datetime 3))))

(deftest datetime-is-within-last-hours-fail
  (let [datetime (-> (java.time.ZonedDateTime/now)
                     (.minusHours 4))]
    (is (false? (datetime-is-within-last-hours? datetime 3)))))

(deftest get-latest-measurement-returns-correct-measurement
  (let [data (get-in sompasauna-response [:body :data])
        latest (get-latest-measurement data)]
    (is (= (:temp_water latest) 30.5))))
