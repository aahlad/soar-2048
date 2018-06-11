(ns soar-2048.core
  (:require [soar-2048.sml :as sml]
            [soar-2048.board :as board]
            [soar-2048.lib :as lib :refer [doto-returnlast]]
            [clojure.data :as data])
  (:gen-class))

(defn add-cell! [agent parent x]
   (doto-returnlast (sml/create-wme! parent "cell")
     (sml/create-wme! "row" (quot x board/row-size))
     (sml/create-wme! "col" (rem x board/row-size))
     (sml/create-wme! "val" 0)))

(defn init-agent-board! [agent parent]
  (doall
   (mapv
    (partial add-cell! agent parent)
    (range board/num-cells))))

(defn update-agent-board! [wme-board board]
  (dorun (map sml/reset-wme! wme-board (map board/to-int board))))

(defn create-il-wmes! [agent]
  (letfn [(create-il-wme! [attr val]
            (sml/create-wme! (sml/input-link agent) attr val))]
    {:score (create-il-wme! "score" 0)
     :round (create-il-wme! "round" 0)
     :board (init-agent-board!
             agent
             (sml/create-wme! (sml/input-link agent) "board"))
     :move-num (create-il-wme! "move-num" 0)
     :status (create-il-wme! "status" "end")}))

(defn zero->nil [x]
  (when-not (zero? x) x))

(defn read-agent-board [il-board]
  (->> il-board
       (map sml/wme-value)
       (map zero->nil)))

(defn process-move! [agent {:keys [board score move-num round status]}]
  (let [cmd (sml/command-by-name agent "move")
        agent-board (read-agent-board board)]
    (when cmd
      (if-let [new-board (board/move agent-board (keyword (sml/parameter-value cmd "direction")))]
        (do (update-agent-board! board new-board)
            (sml/update-wme! move-num inc)
            (sml/update-wme! score (partial + (board/score new-board agent-board))))
        (sml/reset-wme! status "end")))
    (board/string (read-agent-board board))
    (sml/add-status-complete cmd)))

(defn process-new-game! [agent {:keys [board score move-num round status]}]
  (update-agent-board! board (board/new-game))
  (sml/reset-wme! score 0)
  (sml/reset-wme! move-num 1)
  (sml/update-wme! round inc)
  (sml/reset-wme! status "ok"))

(defn prompt-line [prompt]
  (print prompt)
  (flush)
  (read-line))

(defn go! [file]
  (let [kernel (sml/create-kernel)
        agent (sml/create-agent kernel "agent")
        il-wmes (create-il-wmes! agent)]

    ;; initialise agent
    (sml/set-blink-if-no-change agent false)
    (when file
      (sml/load-productions! agent file))

    ;; print callback
    (sml/call-after-print-event
     agent
     (fn [_ _ _ message] (println message)))

    ;; output callback
    (sml/call-after-output-event
     kernel
     (fn [_ _ _ _]
       (when-let [cmd (sml/command-by-name agent "command")]
         ((case (sml/parameter-value cmd "name")
            "move" process-move!
            "new-game" process-new-game!) agent il-wmes)
         (sml/add-status-complete cmd))))

    ;; read input until "q"
    (loop [input (prompt-line "soar> ")]
      (when-not (or (= "q" input) (and (nil? input) (not (println))))
        (print (sml/run-cmd! agent input))
        (recur (prompt-line "soar> "))))
    
    (sml/shutdown kernel)))

(defn -main
  [& [file]]
   (go! file))
