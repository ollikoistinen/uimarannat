(ns uimarannat.core
  (:require [clj-http.client :as http-client])
  (:import [java.util.concurrent TimeoutException TimeUnit]))

(def warmest-swimming-spot (atom nil))
(def ongoing-request-count (atom 0))

(def measurement-expiration-hours 3)

(defn get-index-body []
  (let [swimming-spot-index-url "https://iot.fvh.fi/opendata/uiras/uiras-meta.json"]
    (when-let [index (http-client/get swimming-spot-index-url {:as :json})]
      (:body index))))

(defn get-swimming-spot-keys []
  (when-let [index-body (get-index-body)]
    (keys index-body)))

(defn parse-datetime [datetime-string]
  (try
    (java.time.ZonedDateTime/parse datetime-string java.time.format.DateTimeFormatter/ISO_OFFSET_DATE_TIME)
    (catch Exception e
      (println (str "Parsing datetime failed: " (.getMessage e)))
      (throw e))))

(defn datetime-is-within-last-hours? [datetime hours]
  (let [seconds-in-an-hour   3600
        datetime-since-epoch (.toEpochSecond datetime)
        now-since-epoch      (-> (java.time.ZonedDateTime/now)
                                 .toEpochSecond)]
    (> datetime-since-epoch
       (- now-since-epoch
          (* hours seconds-in-an-hour)))))

(defn get-latest-measurement [measurement-data]
  (->> measurement-data
       (sort-by #(parse-datetime (:time %)))
       last))

(defn print-if-done []
  (when (= @ongoing-request-count 0)
    (let [best-measurement          (get-latest-measurement (:data @warmest-swimming-spot))
          last-measurement-datetime (parse-datetime (:time best-measurement))
          time-delta-seconds        (- (-> (java.time.ZonedDateTime/now) .toEpochSecond)
                                       (-> last-measurement-datetime .toEpochSecond))
          time-delta-minutes        (-> (/ time-delta-seconds 60) int)]
      (println (str "Warmest swimming waters currently found at: " (get-in @warmest-swimming-spot [:meta :name]) " (" (get-in @warmest-swimming-spot [:meta :servicemap_url]) "), water temperature: " (:temp_water best-measurement) ", measured " time-delta-minutes " minutes ago.")))))

(defn process-swimming-spot-response! [response]
  (let [response-body      (:body response)
        measurement-data   (:data response-body)
        latest-measurement (get-latest-measurement measurement-data)]
    (when (datetime-is-within-last-hours? (parse-datetime (:time latest-measurement)) measurement-expiration-hours)
      (swap! warmest-swimming-spot
             (fn [current-best candidate-latest-measurement candidate]
               (if (nil? current-best)
                 candidate
                 (let [current-latest-measurement (get-latest-measurement (:data current-best))]
                   (if (> (:temp_water candidate-latest-measurement)
                          (:temp_water current-latest-measurement))
                     candidate
                     current-best))))
             latest-measurement
             response-body))))

(defn fetch-swimming-spot-details! [spot-key]
  (let [base-url "https://iot.fvh.fi/opendata/uiras/"
        url      (str base-url (name spot-key) "_v1.json")]
    (do
      (swap! ongoing-request-count inc)
      (http-client/get url
                       {:async? true
                        :as     :json}
                       (fn [response] (do
                                       (process-swimming-spot-response! response)
                                       (swap! ongoing-request-count dec)
                                       (print-if-done)))
                       (fn [exception] (do
                                        (swap! ongoing-request-count dec)
                                        (println (str "An error occured while requesting " url ": "  (.getMessage exception)))))))))

(defn fetch-swimming-spots-details! [spot-keys]
  (http-client/with-connection-pool {:timeout 10 :threads 4 :insecure? false :default-per-route 10}
    (run! fetch-swimming-spot-details! spot-keys)))

(defn -main [& args]
  (let [spot-keys (get-swimming-spot-keys)]
    (fetch-swimming-spots-details! spot-keys)))
