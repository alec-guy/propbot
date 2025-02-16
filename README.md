# Propbot Commands

## Overview
Propbot is a tool for validating logical arguments and generating truth tables. Here are the available commands:

## Commands

### `!help`
- **Description:** Displays a list of commands.

### `!check`
- **Description:** Checks the validity of an argument.
- **Syntax:** `!check Premise1, Premise2, PremiseN..., [Therefore symbol] Conclusion`
- **Example:** `!check P, P -> Q, % P`
### `!equiv` 
- **Description:** Checks if two propositions are logically equivalent.
- **Syntax:** `!check Prop1, Prop2`
- **Example:** `!equiv P -> Q, not P or Q`
### `!keyboard`
- **Description:** Outputs all logical operators with their names.

### `!check syntax`
- **Description:** Provides information on how to use the `!check` command properly.

## Symbols for "Therefore"
- `Therefore`
- `therefore`
- `%`
- `∴`

## Examples
- **Basic Check:**
  ```bash
  !check P, P -> Q, % P

If I am offline its because I  the creator, am working or not working on something, which is a tautology. 

Installation link = https://discord.com/oauth2/authorize?client_id=1338255110798704736
