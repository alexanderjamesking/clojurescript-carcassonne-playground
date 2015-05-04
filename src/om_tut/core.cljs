(ns ^:figwheel-always om-tut.core
    (:require [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]
              [domina :as d]
              [clojure.browser.event :as event]
              [goog.string :as gstring]
              [goog.string.format]))

(enable-console-print!)

(def canvas-div (d/by-id "my-canvas"))

(def context 
  (.getContext canvas-div "2d"))

(defonce images
  (for [i (range 1 26)]
    (let [im (new js/Image)
          s (str "images/" (gstring/format "%03.0f" i) ".png")]
      (aset im "src" s)
      im)))
(defonce images-90
  (for [i (range 1 26)]
    (let [im (new js/Image)
          s (str "images/" (gstring/format "%03.0f" i) "-90.png")]
      (aset im "src" s)
      im)))

(defonce images-180
  (for [i (range 1 26)]
    (let [im (new js/Image)
          s (str "images/" (gstring/format "%03.0f" i) "-180.png")]
      (aset im "src" s)
      im)))

(defonce images-270
  (for [i (range 1 26)]
    (let [im (new js/Image)
          s (str "images/" (gstring/format "%03.0f" i) "-270.png")]
      (aset im "src" s)
      im)))

(def tile-height (int 86))
(def tile-width (int 89))

(def xr (map #(* tile-width %) (range 0 10)))
(def yr (map #(* tile-height %) (range 0 10)))

;; define your app data so that it doesn't get over-written on reload

; (defonce app-state (atom {:text "Hello worldy!"}))

; (om/root
;   (fn [data owner]
;     (println data)
;     (reify om/IRender
;       (render [_]
;         (dom/h1 nil (:text data))
;         )))
;   app-state
;   {:target (. js/document (getElementById "app"))})

(defn draw-vertical-line [ctx x-offset]
  (.beginPath ctx)
  (.moveTo ctx x-offset 0)
  (.lineTo ctx x-offset 800)
  (.stroke ctx))

(defn draw-horizontal-line [ctx y-offset]
  (.beginPath ctx)
  (.moveTo ctx 0 y-offset)
  (.lineTo ctx 800 y-offset)
  (.stroke ctx))


(defn draw-image [ctx n x y]
  (let [i (nth images n)]
  (.drawImage ctx i (nth xr x) (nth yr y))))

(defn draw-grid [ctx]
  (doseq [y yr]
    (draw-horizontal-line ctx y))  
  (doseq [x xr]
    (draw-vertical-line ctx x)))

(defn tile [img n e s w] 
  {
    :img img
    :north n
    :south s
    :east e
    :west w
  })

(def tiles [
  (tile  0 :pasture :pasture :pasture :pasture)
  (tile  1 :pasture :pasture :road    :pasture)
  (tile  2 :city    :city    :city    :city   )
  (tile  3 :city    :city    :pasture :city   )
  (tile  4 :city    :city    :pasture :city   )
  (tile  5 :city    :city    :road    :city   )
  (tile  6 :city    :city    :road    :city   )
  (tile  7 :city    :pasture :pasture :city   )
  (tile  8 :city    :pasture :pasture :city   )
  (tile  9 :city    :road    :road    :city   )
  (tile 10 :city    :road    :road    :city   )
  (tile 11 :pasture :city    :pasture :city   )
  (tile 12 :pasture :city    :pasture :city   )
  (tile 13 :city    :pasture :pasture :city   )
  (tile 14 :city    :pasture :city    :pasture)
  (tile 15 :city    :pasture :pasture :pasture)
  (tile 16 :city    :pasture :road    :road   )
  (tile 17 :city    :road    :road    :pasture)
  (tile 18 :city    :road    :road    :road   )
  (tile 19 :city    :road    :pasture :road   )
  (tile 20 :road    :pasture :road    :pasture)
  (tile 21 :pasture :pasture :road    :road   )
  (tile 22 :pasture :road    :road    :road   )
  (tile 23 :road    :road    :road    :road   )
  (tile 24 :city    :road    :pasture :road   )])

(def starting-tile (nth tiles 24))

(def all-tiles [
  (nth tiles 0) (nth tiles 0) (nth tiles 0) (nth tiles 0)
  (nth tiles 1) (nth tiles 1)
  (nth tiles 2)
  (nth tiles 3) (nth tiles 3) (nth tiles 3)
  (nth tiles 4)
  (nth tiles 5)
  (nth tiles 6) (nth tiles 6)
  (nth tiles 7) (nth tiles 7) (nth tiles 7)
  (nth tiles 8) (nth tiles 8)
  (nth tiles 9) (nth tiles 9) (nth tiles 9)
  (nth tiles 10) (nth tiles 10)
  (nth tiles 11)
  (nth tiles 12) (nth tiles 12)
  (nth tiles 13) (nth tiles 13)
  (nth tiles 14) (nth tiles 14) (nth tiles 14)
  (nth tiles 15) (nth tiles 15) (nth tiles 15) (nth tiles 15) (nth tiles 15)
  (nth tiles 16) (nth tiles 16) (nth tiles 16)
  (nth tiles 17) (nth tiles 17) (nth tiles 17)
  (nth tiles 18) (nth tiles 18) (nth tiles 18)
  (nth tiles 19) (nth tiles 19) (nth tiles 19)
  (nth tiles 20) (nth tiles 20) (nth tiles 20) (nth tiles 20) (nth tiles 20) (nth tiles 20) (nth tiles 20) (nth tiles 20)
  (nth tiles 21) (nth tiles 21) (nth tiles 21) (nth tiles 21) (nth tiles 21) (nth tiles 21) (nth tiles 21) (nth tiles 21) (nth tiles 21)
  (nth tiles 22) (nth tiles 22) (nth tiles 22) (nth tiles 22)
  (nth tiles 23)])

(def game-tiles (shuffle all-tiles))

(def counter (atom 0))

(def number-of-tiles (count all-tiles))


(def tiles-on-board (atom []))
(defn add-tile-to-board-data [t x y]
  (swap! tiles-on-board conj { :x x :y y :t t})


  (println @tiles-on-board)
  (println "tiles on board: " (count @tiles-on-board)))

(defn next-tile []
  (let [c @counter]
    (swap! counter inc)
    (if (< c number-of-tiles)
      (nth game-tiles c)
      nil)))

(defn target-loc [x y]
  { :x (Math/floor (/ x tile-width))
    :y (Math/floor (/ y tile-height)) })

(def nt-context 
  (.getContext (d/by-id "next-tile") "2d"))

(def rotation (atom 0))

(defn increase-rotation [v]
  (if (= v 270)
    0
    (+ v 90)))

(defn get-image-to-use [game-tile]
  (let [r @rotation
        image-collection (case r
                               0 images
                               90 images-90
                               180 images-180
                               270 images-270)
        image-to-use (nth image-collection (:img game-tile))]
    image-to-use))

(defn rotate-next-tile []
  (println "rotate NT")
  (println "pre" @rotation)
  (swap! rotation increase-rotation)
  (println "post" @rotation)
  (let [game-tile (nth game-tiles @counter)
        image-to-use (get-image-to-use game-tile)]
    (.drawImage nt-context image-to-use 0 0)
    ))

(event/listen 
  (d/by-id "next-tile") 
  "click"
  #(rotate-next-tile))

(defn draw-next-tile []
  (reset! rotation 0)
  (let [nt (nth game-tiles @counter)
        i (get-image-to-use nt)]
    (.drawImage nt-context i 0 0)))

(defn draw-tile [t]
  (let [nt (next-tile)]
    (if (nil? nt)
      (println "GAME OVER")
      (let [iii (get-image-to-use nt)] 
        (.drawImage context iii (nth xr (:x t)) (nth yr (:y t)))
        (add-tile-to-board-data nt (:x t) (:y t))
        (draw-next-tile)))))

(defn init-canvas-click-listener []
  (event/listen 
    canvas-div 
    "click"
    (fn [e] 
      (let [x (aget e "offsetX")
            y (aget e "offsetY")
            tl (target-loc x y)]
        (draw-tile tl)))))

(defn render-first-tile []
  (draw-image context (:img starting-tile) 4 3)
  (add-tile-to-board-data starting-tile 4 3))

(defn render-canvas []
  (let [ctx context]
    (draw-grid ctx)
    (render-first-tile)
    (draw-next-tile)
    (init-canvas-click-listener)
    ))

(set! (.-onload js/window) render-canvas)

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  (println "js reloaded")
  (render-canvas)


  )

