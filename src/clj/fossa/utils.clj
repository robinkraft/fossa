(ns fossa.utils)

(defn split-line
  "Returns vector of line values by splitting on tab."
  [line]
  (vec (.split line "\t")))

(defn mk-latlon-str
  "Make a comma-separated latlon string given lat and lon"
  [lat lon]
  (str lat "," lon))

(defn mk-latlon-map
  "Make a map with a latlon string as key and a value of [] (if no value
   supplied) or [val]"
  ([lat lon]
     {(mk-latlon-str lat lon) []})
  ([lat lon val]
     {(mk-latlon-str lat lon) [val]}))

(defn mk-empty-latlon-map
  "Makes a latlon map with empty vector as value"
  [lats lons]
  (apply merge (map mk-latlon-map lats lons)))

(defn collect-latlon-vals->map
  "Creates a map with latlon strings as keys. For values, if there are multiple
   entries for a given latlon, concatenates the entries together in a list.

   Usage:
     (collect-latlon-vals->map [1 2 1] [3 4 3] [10 11 12])
     ;=> {\"2,4\" (11), \"1,3\" (10 12)}"
  ([lats lons values]
     (let [merge-fn (comp vec concat)
           empty-map (mk-empty-latlon-map lats lons)
           ms (map mk-latlon-map lats lons values)]
       (apply merge-with merge-fn empty-map ms))))

(defn latlon->coord-str
  "Given a latitude and longitude, returns a WKT-formatted \"(lon lat)\"
   string."
  ([lat lon]
     (let [coord-str "%s %s"]
       (format coord-str lon lat)))
  ([[lat lon]]
     (latlon->coord-str lat lon)))

(defn latlons->wkt-multi-point
  "Returns WKT MULTIPOINT string from supplied lats and lons."
  [lats lons]
  (let [out-str "MULTIPOINT (%s)"
        sep ", "]
    (->> (map vector lats lons)
         (vec)
         (map latlon->coord-str)
         (interpose sep)
         (apply str)
         (format out-str))))

(defn mk-sorted-map
  "Make a map a sorted map."
  [m]
  (into (sorted-map) m))

(defn parse-for-wkt
  "Parse latlons for well known text, ensuring that order is retained
   using an ordered map, as for other data fields.

   Use of maps also ensures all unique latlons in output."
  [lats lons]
  (let [latlons (keys (mk-sorted-map (mk-empty-latlon-map lats lons)))
        latlon-split (map #(.split % ",") latlons)]
    (latlons->wkt-multi-point (map first latlon-split)
                              (map second latlon-split))))

(defn extract
  "Extract a specific field by index from tuples."
  [field tuples]
  (map #(nth % field) tuples))

(defn extract-field
  "Extracts a particular field (indexed by field-num), creates an
   ordered map with \"lat,lon\" keys, and returns the values in order."
  [tuples lats lons field-num]
  (->> (extract field-num tuples)
       (collect-latlon-vals->map lats lons)
       (mk-sorted-map)
       (vals)))

(defn valid-name?
  "Return true if name is valid, otherwise return false."
  [name]
  (and (not= name nil) (not= name "")))

(defn latlon-valid?
  "Return true if lat and lon are valid, otherwise return false."
  [lat lon]
  (let [latlon-range {:lat-min -90 :lat-max 90 :lon-min -180 :lon-max 180}]
    (try
      (let [{:keys [lat-min lat-max lon-min lon-max]} latlon-range]
        (and (<= lat lat-max)
             (>= lat lat-min)
             (<= lon lon-max)
             (>= lon lon-min)))
      (catch Exception e false))))

(defn cleanup-slash-N
  "Replace \\N with empty string in precision field."
  [s]
  (if (= s "\\N") "" s))

(defn parse-hemisphere
  "Returns a quarter->season map based on the hemisphere."
  [h]
  (let [n_seasons {0 "winter" 1 "spring" 2 "summer" 3 "fall"}
        s_seasons {0 "summer" 1 "fall" 2 "winter" 3 "spring"}]
    (if (= h "N") n_seasons s_seasons)))

(defn get-season-idx
  "Returns season index given a month."
  [month]
  {:pre [(>= 12 month)]}
  (let [season-idxs {11 0 12 0 1 0
                     2 1 3 1 4 1
                     5 2 6 2 7 2
                     8 3 9 3 10 3}]
    (get season-idxs month)))

(defn get-season
  "Based on the latitude and the month, return a \"hemisphere season\"
   string.

   Usage:
     (get-season 40.0 1)
     ;=> \"N winter\""
  [lat month]
  (let [lat (if (string? lat) (read-string lat) lat)
        month (if (string? month) (read-string month) month)
        hemisphere (if (pos? lat) "N" "S")
        season (get (parse-hemisphere hemisphere)
                    (get-season-idx month))]
    (format "%s %s" hemisphere season)))
