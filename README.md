# Propbot Commands
![Prismo](https://github.com/user-attachments/assets/a888f552-d581-4ffa-8cb7-ffe096d42755)
## Overview
Propbot is a tool for validating logical arguments and generating truth tables. Here are the available commands:

## Commands

### `!help`
- **Description:** Displays a list of commands.

### `!check`
- **Description:** Checks the validity of an argument.
- **Syntax:** `!check Premise1, Premise2, PremiseN..., [Therefore symbol] Conclusion`
- **Example:** `!check P, P -> Q, % P`

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
