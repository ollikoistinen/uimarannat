(ns uimarannat.core
  (:require [org.httpkit.client :as http]
            [cheshire.core :as cheshire])
  (:import [java.util.concurrent TimeoutException TimeUnit]))

(defn get-index-body []
  (let [swimming-spot-index-url "https://iot.fvh.fi/opendata/uiras/uiras-meta.json"
        index                   (http/get swimming-spot-index-url)]
    (-> @index
        :body
        (cheshire/parse-string true))))

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

(defn print-swimming-spot [swimming-spot]
  (let [latest-measurement        (get-latest-measurement (:data swimming-spot))
        last-measurement-datetime (parse-datetime (:time latest-measurement))
        time-delta-seconds        (- (-> (java.time.ZonedDateTime/now) .toEpochSecond)
                                     (-> last-measurement-datetime .toEpochSecond))
        time-delta-minutes        (-> (/ time-delta-seconds 60) int)]
    (println (str "Warmest swimming waters currently found at: " (get-in swimming-spot [:meta :name]) " (" (get-in swimming-spot [:meta :servicemap_url]) "), water temperature: " (:temp_water latest-measurement) ", measured " time-delta-minutes " minutes ago."))))

(defn fetch-swimming-spots-details [spot-keys]
  (let [base-url "https://iot.fvh.fi/opendata/uiras/"
        make-url (fn [spot-key] (str base-url (name spot-key) "_v1.json"))
        urls     (map make-url spot-keys)
        promises (doall (map http/get urls))
        results  (map deref promises)]
    (->> results
         (map (fn [{:keys [status body]}]
                (when (= status 200)
                  (cheshire/parse-string body true))))
         (filter some?))))

(defn get-best-swimming-spot [swimming-spots measurement-expiration-hours]
  (reduce
    (fn [a b]
      (if (not (datetime-is-within-last-hours? (-> b :data get-latest-measurement :time parse-datetime)
                                               measurement-expiration-hours))
        a
        (if (> (-> b :data get-latest-measurement :temp_water)
               (-> a :data get-latest-measurement :temp_water))
          b
          a)))
    swimming-spots))

(defn -main [& args]
  (let [measurement-expiration-hours 3
        spot-keys                    (get-swimming-spot-keys)]
    (-> spot-keys
        fetch-swimming-spots-details
        (get-best-swimming-spot measurement-expiration-hours)
        print-swimming-spot)))
