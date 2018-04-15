# WCQ
Replication data and code for "How effectively does World Cup qualification performance predict success in the World Cup finals?"

Created by Julian Gerez

### wc_match_data.csv
| Variable | Description |
|--|--|
| id | Type of match (finals/qualifier) |
| tourn_id | World Cup cycle match was played in |
| date | Date of match (yyyy-mm-dd)|
| teamA | TeamA name |
| scoreA | Score of teamA |
| scoreB | Score of teamB |
| teamB | TeamB name |
| et | Extra time dummy |
| sc | Special circumstances dummy (for certain qualifier matches that were annulled or abandoned |
| w | Team that won match |
| l | Team that lost match |
| d | True if draw, else false |
| dA | If draw, name of teamA |
| dB | If draw, name of teamB |

### wc_results_data.csv
| Variable | Description |
|--|--|
| tourn_id | World Cup cycle |
| team | Country name |
| qgp | Qualifier games played |
| qw | Qualifier wins |
| qd | Qualifier draws |
| ql | Qualifier losses |
| qp | Qualifier points |
| qpp | Qualifier points possible (qw\*3 + qd\*1) / qgp |
| qop | Qualifier old points |
| qopp | Qualifier old points possible (qw\*2 + qd\*1) / qgp |
| qgf | Qualifier goals for |
| qga | Qualifier goals against |
| qgd | Qualifier goal difference |
| fgp, fw, fd...| Same variables but for World Cup finals matches
| hd | Host dummy (1 = host) |
| cd | Champion dummy (1 = champion) |
