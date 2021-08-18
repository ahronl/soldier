(ns soldier.core)

(defn is-location-equal?
  [l1 l2]
  (and (= (:x l1) (:x l2)) (= (:y l1) (:y l2))))

(defn mk-soldier 
  [name & {:keys [life location] :or {life 100 location {:x 0 :y 0}}}]
  {:name name :location location :strenght 10 :life life})

(defn is-dead?
  [s]
  (> 1 (:life s)))

(defn- go
  [s dir size]
  (let [d (if (contains? #{:left :right} dir) :x :y)
        f (if (contains? #{:left :up} dir) dec inc)
        next-pos (f (get-in s [:location d]))]
     (if (and (> size next-pos) (not (neg? next-pos)))
        (update-in s [:location d] f)
        s)))

(defn go-left
  [s size]
  (go s :left size))

(defn go-up
  [s size]
  (go s :up size))

(defn go-right
  [s size]
  (go s :right size))

(defn go-down
  [s size]
  (go s :down size))

(defn fight
 [sol1 sol2]
 (let [attack (fn [attacker attacked] 
                  (let [punch (rand-int (:strenght @attacker))]
                    (swap! attacked update :life - punch)))
       s1 (atom sol1)
       s2 (atom sol2)]
   (do 
    (while (every? false? (map is-dead? [@s1 @s2]))
      (attack s1 s2)
      (println (:name @s1) " attacked --> " (:name @s2) " life: " (:life @s2))
      (attack s2 s1))
      (println (:name @s2) " attacked --> " (:name @s1) " life: " (:life @s1))
    (if (is-dead? @s1)
      {:win @s2 :loose @s1}
      {:win @s1 :loose @s2}))))

(defn game 
  [s1-name s2-name size]
  (let [s1 (atom (mk-soldier s1-name :location {:x (dec size) :y 0}))
        s2 (atom (mk-soldier s2-name :location {:x 0 :y (dec size)}))
        random-move (fn [s size]
                        (let [m (rand-int 4)]
                          (case m
                            0 (go-up s size)
                            1 (go-down s size)
                            2 (go-left s size)
                            3 (go-right s size))))]
        (do
          (while (not (is-location-equal? (:location @s1) (:location @s2)))
             (swap! s1 random-move size)
             (println s1-name " moved --> " @s1)
             (swap! s2 random-move size)
             (println s2-name " moved --> " @s2))
          (fight @s1 @s2))))
