#lang racket

(require racket/vector)
(require racket/gui/base)

(include "canvas_to_ai.ss")
(include "global_variables.ss")
(include "vector_board.ss")
(include "move_making.ss")
(include "gui_elements.ss")
(include "random.ss")
(include "alphabeta.ss")

(open-game)