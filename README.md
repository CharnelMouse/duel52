A program for playing Duel 52, to practise domain modelling for card games.

Work in progress: UI in place, basic play rules implemented. Need to implement card abilities.

- Basic actions: play card, activate, attack, make pair. Cards ready at the end of the turn.
- Players only see known information (except who's peeked at face-down cards).
- Bases become activatable when the draw pile empties.
- Lanes and lanes can be won, or tied.
  - Lost/tied lanes can still be played into if a player has cards in their hands, making them contested again.
  - "As long as you have cards in your hand, the lanes are still 'alive', and you can play cards into them."
  - I'm not sure what the implication is if you win a lane, everyone (except you) has an empty hand, and you move your last card out of the lane.
- Card abilities remaining to implement:
  - View
  - Trap
  - Foresight
  - Flip
  - Freeze
  - Heal
  - Retaliate
  - Nimble
  - TwinStrike
  - Taunt
  - Vampiric (replaces Trap and Foresight in solo mode)
  - Move
  - Empower
  - Action


Future extensions:

- Variants with 1, 3, or 4 players
- Expansion: Heroes, Bannermen, Forts
