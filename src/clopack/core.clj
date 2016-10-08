(ns clopack.core)

(require '[clopack-native.core :as cn])

; Deserialization functions

(defn- extract-integer [data size]
  (let [slice (take size data)]
    (reduce bit-and slice)))

(defn- extract-string [data size]
  (let [slice (take size data)]
    (apply str (map #(char (bit-and % 255)) slice))))

(defn- deserialize' [data form packet]
  (let [[id size kind] (take 3 form)
        value (case kind
                :integer (extract-integer data size)
                :string (extract-string data size))
        data (drop size data)
        form (drop 3 form)
        packet (assoc packet id value)]
    (deserialize' data form packet)))

(defn- deserialize [data form]
  (when (mod (count form) 3)
    (deserialize' data form {})))

; Handler functions.

(defn create-handler
  "Creates a link-layer packet handler with the given
  interface, form, filter condition and actions.
  Returns a handler object on success, nil otherwise."
  [interface form condition actions]
  (if-let [ctx (cn/create-context interface)]
    {:context ctx :form form :condition condition :actions actions}))

; some horrible shit right here

(defn- apply-handler-actions [packet]
  (let [[condition-or-action action] (take 2 packet)]
    (case [condition-or-action action]
      [nil nil] nil
      [_ nil]   (condition-or-action packet)
      [_ _]     (if (condition-or-action packet)
                  (action packet)))))

(defn run-handler
  "Runs a data frame handler once.
  Returns true if a packet was handled, false if not,
  and nil if an error occurred."
  [handler]
  (let [ctx       (:context handler)
        form      (:form handler)
        condition (:condition handler)
        actions   (:actions handler)
        buf       (cn/read-frame ctx)
        packet    (deserialize buf form)]
    (case [buf (condition packet)]
      [_ true]  (do
                  (apply-handler-actions packet)
                  true)
      [_ false] false)))
