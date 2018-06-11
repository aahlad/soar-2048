(ns soar-2048.sml
  (:require [soar-2048.lib :as lib :refer [if-return-else]])
  (:import [sml
            Agent
            Agent$PrintEventInterface
            Kernel
            Kernel$UpdateEventInterface
            Identifier
            smlUpdateEventId
            smlPrintEventId]))

(defn create-kernel []
  (let [kernel (Kernel/CreateKernelInNewThread)]
    (if (.HadError kernel)
      (throw (Exception. "sml: Error creating Soar kernel"))
      kernel)))

(defn create-agent [kernel name]
  (let [agent (.CreateAgent kernel name)]
    (if-return-else agent
      (throw (Exception. "sml: Error creating Soar agent")))))

(defmulti create-wme!
  (fn [_ _ & [val]] (class val)))

(defmethod create-wme! Long [parent attr val]
  (.CreateIntWME parent attr val))

(defmethod create-wme! String [parent attr val]
  (.CreateStringWME parent attr val))

(defmethod create-wme! Double [parent attr val]
  (.CreateFloatWME parent attr val))

(defmethod create-wme! nil [parent attr]
  (.CreateIdWME parent attr))

(defn wme-value [wme]
  (.GetValue wme))

(defn parameter-value [parent attr]
  (.GetParameterValue parent attr))

(defn reset-wme! [wme val]
  (.Update wme val))

(defn update-wme! [wme f]
  (.Update wme (f (wme-value wme))))

(defn run-cmd! [agent command]
  (.ExecuteCommandLine agent command))

(defn load-productions! [agent file]
  (.LoadProductions agent file)
  (when (.HadError agent)
    (throw (Exception. "sml: Error loading productions"))))

(defn input-link [agent]
  (.GetInputLink agent))

(defn shutdown [kernel]
  (.Shutdown kernel))

(defn run-agents-forever [kernel]
  (.RunAllAgentsForever kernel))

(defn call-after-output-event [kernel f]
  (.RegisterForUpdateEvent
   kernel
   smlUpdateEventId/smlEVENT_AFTER_ALL_OUTPUT_PHASES
   (reify Kernel$UpdateEventInterface
     (updateEventHandler [this event-id data kernel run-flags]
       (f event-id data kernel run-flags)))
   nil))

(defn call-after-print-event [agent f]
  (.RegisterForPrintEvent
   agent
   smlPrintEventId/smlEVENT_PRINT
   (reify Agent$PrintEventInterface
     (printEventHandler [this event-id data agent message]
       (f event-id data agent message)))
   nil))

(defn output-link [agent]
  (.GetOutputLink agent))

(defn command-by-name [agent cmd]
  (->> agent
       .GetNumberCommands
       range
       (map #(.GetCommand agent %))
       (filter #(= cmd (.GetCommandName %)))
       first))

(defn add-status-complete [command]
  (.AddStatusComplete command))

(defn set-blink-if-no-change [agent b]
  (.SetBlinkIfNoChange agent b))
