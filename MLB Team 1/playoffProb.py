import random as rand
from collections import Counter

def simulate_season():

    # initialize records
    records = {
        "Padres": [91, 66],
        "Mets": [87, 70],
        "Diamondbacks": [87, 71],
        "Braves": [86, 71],
        "Random Team": [0, 0]
    }

    # game simulator and record updating
    def game_simulator(team1, team2):
        if rand.random() < 0.5:
            records[team1][0] += 1
            records[team2][1] += 1
        else:
            records[team2][0] += 1
            records[team1][1] += 1

    # sim braves vs mets x2
    for _ in range(2):
        game_simulator("Mets", "Braves")

    # sim dbacks vs padres x3
    for _ in range(3):
        game_simulator("Diamondbacks", "Padres")

    # simulate remaining games against others
    for team in ["Braves", "Mets"]:
        for _ in range(3):
            game_simulator(team, "Random Team")

    for team in ["Padres"]:
        for _ in range(2):
            game_simulator(team, "Random Team")

    for team in ["Diamondbacks"]:
        for _ in range(1):
            game_simulator(team, "Random Team")

    # final standings + sorting
    standings = sorted(records.items(), key=lambda x: x[1][0], reverse=True)
    return [team[0] for team in standings]  # Return only team names

# run sims
num_simulations = 1_000_000
mets_1 = 0
mets_2 = 0
mets_3 = 0
mets_4 = 0
standings_counter = Counter()

for _ in range(num_simulations):
    final_standings = simulate_season()
    standings_counter[tuple(final_standings)] += 1
    if final_standings[0] == "Mets":
        mets_1 += 1
    elif final_standings[1] == "Mets":
        mets_2 += 1
    elif final_standings[2] == "Mets":
        mets_3 += 1
    elif final_standings[3] == "Mets":
        mets_4 += 1

# Calculate percentages for each position
percentage_mets_1 = (mets_1 / num_simulations) * 100
percentage_mets_2 = (mets_2 / num_simulations) * 100
percentage_mets_3 = (mets_3 / num_simulations) * 100
percentage_mets_4 = (mets_4 / num_simulations) * 100

# Output results
print(f"Percentage of times Mets finish 1st: {percentage_mets_1:.2f}%")
print(f"Percentage of times Mets finish 2nd: {percentage_mets_2:.2f}%")
print(f"Percentage of times Mets finish 3rd: {percentage_mets_3:.2f}%")
print(f"Percentage of times Mets finish 4th: {percentage_mets_4:.2f}%")
