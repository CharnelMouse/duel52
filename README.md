A program for playing Duel 52, to practise domain modelling for card games.

Work in progress: primitive UI in place, basic play rules implemented. Need to implement card abilities.

- Basic actions: play card, activate, attack, make pair. Cards ready at the end of the turn.
- Players only see known information (except who's peeked at face-down cards, the console UI is verbose enough as it is).
- Bases become activatable when the draw pile empties.
- You can look at your bases if they die face-down (to check whether they're a Trap). [need to check whether implemented]
- After the draw pile runs out, lanes are counted as won while only one player has cards there.
- Lanes have no "memory" WRT being won: a won lane is no longer won if it gets emptied by Move powers.
- We only check for death after resolving all triggered abilities. Specific rulings motivating this:
  - A Vampiric card on 1 health killing a Retaliate survives.
  - Dying Taunt cards still protect non-Taunt cards in that lane.
- Won lane counts are only used to check for victory once the player hands are all empty, since lanes cannot then be un-won.
  - Strictly speaking, the game ends when no one can take an action, but checking when hands are empty speeds things up.
  - We can only check early under the assumption that you can't be forced to vacate a won lane. For example, the Move power is always optional. This assumption might change if we ever allow the use of variant power sets.
- Card ability implementation progress:
  - View
    - Draw a card [implemented]
    - Choose a card to discard [implemented]
  - Trap [implemented]
  - Foresight [implemented]
  - Flip
    - Activate all inactive allied cards in lane [implemented]
    - Choose order to resolve triggered activation powers [implemented]
  - Freeze [implemented]
  - Heal [implemented]
  - Retaliate [implemented]
  - Nimble
    - Immune to Freeze [implemented]
    - Immune to Retaliate [implemented]
    - Can't be secondary target for Twinstrike [implemented]
    - Blocks Twinstrike ability if primary target [not implemented]
    - Extra damage to Taunt (can't attack past it) [implemented]
  - TwinStrike [implemented]
    - Trap cards killed by Twinstrike damage correctly flip and heal
    - Twinstrike damage gets correctly blocked after killing a Taunt card
    - Takes damage if dealing Twinstrike damage to a Retaliate (only one of a pair) [not implemented]
  - Taunt
    - 3 maximum health instead of 2 [implemented]
    - If present, non-Taunt allies can't be attacked by non-Nimble enemies [implemented]
    - If present, non-Taunt allies can't be secondary target for Twinstrike either [implemented]
  - Vampiric (replaces Trap and Foresight in solo mode) [implemented]
  - Move [implemented]
  - Empower [implemented]
  - Action
    - Gain 1 action [implemented]
    - This card has a maximum of 2 attacks this turn [implemented]

Future extensions:

- Variants with 1, 3, or 4 players
- Expansion: Heroes, Bannermen, Forts
