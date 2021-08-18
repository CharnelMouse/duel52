A program for playing Duel 52, to practise domain modelling for card games.

Work in progress: UI in place, basic play rules implemented. Need to implement card abilities.

- Basic actions: play card, activate, attack, make pair. Cards ready at the end of the turn.
- Players only see known information (except who's peeked at face-down cards).
- Bases become activatable when the draw pile empties.
- After the draw pile runs out, lanes are counted as won while only one player has cards there.
- Lanes have no "memory" WRT being won: a won lane is no longer won if it gets emptied by Move powers.
- A Vampiric card on 1 health killing a Retaliate survives, and dying Taunt cards still protect non-Taunt cards in that lane, so current working model is that we only check for deaths after resolving all triggered abilities.
- Won lane counts are only used to check for victory once the player hands are all empty, since lanes cannot then be un-won.
  - Strictly speaking, the game ends when no one can take an action, but checking when hands are empty speeds things up.
- Card ability implementation progress:
  - View
    - Draw a card [implemented]
    - Choose a card to discard [implemented]
  - Trap [implemented]
  - Foresight [implemented]
  - Flip
    - Activate all inactive allied cards in lane [to implement]
    - Choose order to resolve triggered activation powers [to implement]
  - Freeze [implemented]
  - Heal [implemented]
  - Retaliate [implemented]
  - Nimble
    - Immune to Freeze [implemented]
    - Immune to Retaliate [implemented]
    - Can't be secondary target for Twinstrike [implemented]
    - Extra damage to Taunt [implemented]
  - TwinStrike [implemented]
  - Taunt
    - 3 maximum health instead of 2 [implemented]
    - If present, non-Taunt allies can't be attacked by non-Nimble enemies [to implement]
    - If present, non-Taunt allies can't be secondary target for Twinstrike either [to implement]
  - Vampiric (replaces Trap and Foresight in solo mode) [to implement]
  - Move [to implement]
  - Empower [to implement]
  - Action
    - Gain 1 action [to implement]
    - This card has a maximum of 2 attacks this turn [to implement]

Future extensions:

- Variants with 1, 3, or 4 players
- Expansion: Heroes, Bannermen, Forts
