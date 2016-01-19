package me.reminisce.gameboard

/**
  * Provides the actors and classes definition for the board layout creation.
  *
  * ==Overview==
  * - [[me.reminisce.gameboard.board.GameGenerator]] is the entry point, it coordinates the data refresh and the
  * board generation
  * - [[me.reminisce.gameboard.board.BoardGenerator]] defines the basic behavior of any board generator and also defines
  * some useful functions which can be helpful to the subclasses.
  * - [[me.reminisce.gameboard.board.RandomBoardGenerator]] defines an abstract random generation strategy
  * - [[me.reminisce.gameboard.board.UniformBoardGenerator]], [[me.reminisce.gameboard.board.FullRandomBoardGenerator]],
  * [[me.reminisce.gameboard.board.StrategyChooser]] are implementations of board generators.
  * - [[me.reminisce.gameboard.board.TileGenerator]] is a worker which handles the creation of a single tile
  */
package object board
