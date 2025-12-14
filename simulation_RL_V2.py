# %%
"""
Python translation of simulation_RL_V2.R
Provides a simulation runner for Q-learning models and wrappers for different model specifications
Outputs pandas DataFrames analogous to the R version (choices, probs, Q_history, proportions)
"""
import numpy as np
import pandas as pd
from typing import Dict, Any, List, Optional
import matplotlib.pyplot as plt


def _expand_param(x, n_arms, default=0.0):
    if x is None:
        return np.repeat(default, n_arms)
    if isinstance(x, (int, float)):
        return np.repeat(float(x), n_arms)
    # If a dict was provided, extract its values (not keys)
    if isinstance(x, dict):
        vals = list(x.values())
        try:
            arr = np.array(vals, dtype=float)
        except Exception:
            # fallback: try converting strings that contain numbers
            arr = np.array([float(v) for v in vals], dtype=float)
    else:
        # list/tuple/np array
        arr = np.array(list(x), dtype=float)

    if arr.size == 1:
        return np.repeat(float(arr[0]), n_arms)
    # if longer, truncate or pad
    if arr.size >= n_arms:
        return arr[:n_arms]
    pad = np.repeat(default, n_arms - arr.size)
    return np.concatenate([arr, pad])


def simulation_runner_rl(n_choices: int, options: Dict[str, Dict[str, Any]], params: Dict[str, Any], model_name: str = "undefined") -> Dict[str, Any]:
    n_arms = len(options)

    Q_values = np.zeros(n_arms, dtype=float)
    Q_history = np.full((n_choices, n_arms), np.nan, dtype=float)

    choices = np.full(n_choices, -1, dtype=int)
    rewards = np.full(n_choices, np.nan, dtype=float)
    probs_history = np.full((n_choices, n_arms), np.nan, dtype=float)

    alphas = params.get("alphas")
    forgets = params.get("forgets")
    lambdas = params.get("lambdas")
    rhos = params.get("rhos")

    lambda_vec = _expand_param(lambdas, n_arms, default=1.0)
    forget_vec = _expand_param(forgets, n_arms, default=0.0)

    # alpha handling
    if alphas is None:
        alpha_gain = np.repeat(0.1, n_arms)
        alpha_loss = np.repeat(0.1, n_arms)
    else:
        if isinstance(alphas, dict):
            if "alpha" in alphas and len(alphas) == 1:
                alpha_gain = alpha_loss = _expand_param(alphas["alpha"], n_arms, default=0.1)
            elif "alpha_loss" in alphas and "alpha_gain" in alphas:
                alpha_loss = _expand_param(alphas["alpha_loss"], n_arms, default=0.1)
                alpha_gain = _expand_param(alphas["alpha_gain"], n_arms, default=0.1)
            else:
                # dict of named values -> try to extract loss/gain
                vals = list(alphas.values())
                arr = np.array(vals, dtype=float)
                if arr.size == n_arms:
                    alpha_gain = alpha_loss = arr
                else:
                    alpha_gain = alpha_loss = _expand_param(arr, n_arms, default=0.1)
        else:
            # list/tuple/np array
            arr = np.array(alphas, dtype=float)
            if arr.size == n_arms:
                alpha_gain = alpha_loss = arr
            elif arr.size == 2:
                alpha_loss = _expand_param(arr[0], n_arms, default=0.1)
                alpha_gain = _expand_param(arr[1], n_arms, default=0.1)
            else:
                alpha_gain = alpha_loss = _expand_param(arr[0], n_arms, default=0.1)

    rho_JP = 0.0
    rho_BS = 0.0
    if rhos is not None:
        if isinstance(rhos, dict):
            rho_JP = float(rhos.get("rho_JP", 0.0))
            rho_BS = float(rhos.get("rho_BS", 0.0))
        else:
            arr = np.array(rhos, dtype=float)
            if arr.size >= 2:
                rho_BS, rho_JP = arr[0], arr[1]

    # convert options to indexed list for convenience
    opts = list(options.values())

    for t in range(n_choices):
        V = lambda_vec * Q_values
        # apply rhos using the same mapping as R script
        if rhos is not None:
            if n_arms >= 1:
                V[0] = V[0] + rho_JP
            if n_arms >= 3:
                V[2] = V[2] + rho_BS
            if n_arms >= 4:
                V[3] = V[3] + rho_BS + rho_JP

        # softmax stable
        vmax = np.max(V)
        expV = np.exp(V - vmax)
        probs = expV / np.sum(expV)
        probs = np.maximum(probs, 1e-10)
        probs = probs / np.sum(probs)

        choice = np.random.choice(np.arange(1, n_arms + 1), p=probs)
        opt = opts[choice - 1]

        u = np.random.rand()
        jp_p = float(opt.get("p_jp", 0.0))
        bs_p = float(opt.get("p_bs", 0.0))

        if u < jp_p:
            reward = float(opt.get("jp", 0.0))
        elif u < jp_p + bs_p:
            reward = float(opt.get("bs", 0.0))
        else:
            # choose normal gain or loss
            if np.random.rand() < 0.5:
                reward = float(opt["gain"][t])
            else:
                reward = float(opt["loss"][t])

        probs_history[t, :] = probs
        choices[t] = choice
        rewards[t] = reward

        # choose alpha
        if reward >= 0:
            alpha_used = alpha_gain[choice - 1]
        else:
            alpha_used = alpha_loss[choice - 1]

        pe = reward - Q_values[choice - 1]
        Q_values[choice - 1] = Q_values[choice - 1] + alpha_used * pe

        not_chosen = [i for i in range(n_arms) if (i + 1) != choice]
        Q_values[not_chosen] = Q_values[not_chosen] * (1.0 - forget_vec[not_chosen])

        Q_history[t, :] = Q_values

    choices_df = pd.DataFrame({
        "trial": np.arange(1, n_choices + 1),
        "choice": choices,
        "reward": rewards
    })

    probs_df = pd.DataFrame(probs_history, columns=[f"p{i+1}" for i in range(n_arms)])
    probs_df["trial"] = np.arange(1, n_choices + 1)

    Q_history_df = pd.DataFrame(Q_history, columns=[f"Q{i+1}" for i in range(n_arms)])
    Q_history_df["trial"] = np.arange(1, n_choices + 1)

    proportions = pd.DataFrame({
        "Iteration": np.arange(1, n_choices + 1),
        "Antifragile": np.cumsum(choices == 1) / np.arange(1, n_choices + 1),
        "Robust": np.cumsum(choices == 2) / np.arange(1, n_choices + 1),
        "Fragil": np.cumsum(choices == 3) / np.arange(1, n_choices + 1),
        "Vulnerable": np.cumsum(choices == 4) / np.arange(1, n_choices + 1),
    })

    return {
        "model": model_name,
        "params": params,
        "choices": choices_df,
        "probs": probs_df,
        "Q_history": Q_history_df,
        "proportions": proportions,
    }


# wrapper functions
def simulation_homogeneous_rl(n_choices, options, alpha, forget, lambda_):
    params = {"alphas": {"alpha": alpha}, "forgets": {"forget": forget}, "lambdas": {"lambda": lambda_}}
    return simulation_runner_rl(n_choices=n_choices, options=options, params=params, model_name="HOMOGENEOUS")


def simulation_gain_loss_rl(n_choices, options, alpha_loss, alpha_gain, forget, lambda_):
    params = {"alphas": {"alpha_loss": alpha_loss, "alpha_gain": alpha_gain}, "forgets": {"forget": forget}, "lambdas": {"lambda": lambda_}}
    return simulation_runner_rl(n_choices=n_choices, options=options, params=params, model_name="GAIN_LOSS")


def simulation_biased_rl(n_choices, options, alpha_loss, alpha_gain, forgets_vec, lambdas_vec):
    params = {"alphas": {"alpha_loss": alpha_loss, "alpha_gain": alpha_gain}, "forgets": forgets_vec, "lambdas": lambdas_vec}
    return simulation_runner_rl(n_choices=n_choices, options=options, params=params, model_name="BIASED")


def simulation_ree_biased_simple_rl(n_choices, options, alpha_l, alpha_g, rho_BS, rho_JP, forget, lambda_):
    params = {"alphas": {"alpha_loss": alpha_l, "alpha_gain": alpha_g}, "forgets": {"forget": forget}, "lambdas": {"lambda": lambda_}, "rhos": {"rho_BS": rho_BS, "rho_JP": rho_JP}}
    return simulation_runner_rl(n_choices=n_choices, options=options, params=params, model_name="REE_BIASED_SIMPLE")


def simulation_ree_learning_simple_rl(n_choices, options, alpha1, alpha2, alpha3, alpha4, forget, lambda_):
    params = {"alphas": [alpha1, alpha2, alpha3, alpha4], "forgets": {"forget": forget}, "lambdas": {"lambda": lambda_}}
    return simulation_runner_rl(n_choices=n_choices, options=options, params=params, model_name="REE_LEARNING_SIMPLE")


def simulation_ree_learning_biased_simple_rl(n_choices, options, alpha1, alpha2, alpha3, alpha4, forget, lambda_, rho_BS, rho_JP):
    params = {"alphas": [alpha1, alpha2, alpha3, alpha4], "forgets": {"forget": forget}, "lambdas": {"lambda": lambda_}, "rhos": {"rho_BS": rho_BS, "rho_JP": rho_JP}}
    return simulation_runner_rl(n_choices=n_choices, options=options, params=params, model_name="REE_LEARNING_BIASED_SIMPLE")

# small helpers
def compute_TSREE(proportions_df: pd.DataFrame) -> np.ndarray:
    return 1 + proportions_df["Antifragile"].values - proportions_df["Fragil"].values


def compute_OSSREE(proportions_df: pd.DataFrame) -> np.ndarray:
    return proportions_df["Vulnerable"].values - proportions_df["Robust"].values


def plot_TSREE_OSSREE(proportions_df: pd.DataFrame):
    OSSREE = compute_OSSREE(proportions_df)
    TSREE = compute_TSREE(proportions_df)
    plt.figure()
    plt.plot(OSSREE, TSREE, color="darkblue")
    plt.plot([0, 1, 0, -1, 0], [0, 1, 2, 1, 0], color="black")
    plt.axvline(0, linestyle="--", color="gray")
    plt.axhline(1, linestyle="--", color="gray")
    plt.xlabel("OSSREE")
    plt.ylabel("TSREE")
    plt.title("Evolution of TSREE and OSSREE over trials")
    plt.show()

#%%

if __name__ == "__main__":
    # example usage
    n_choices = 500
    options = {
        "option1": {"gain": np.random.choice([3, 4], n_choices), "loss": np.random.choice([-9, -8], n_choices), "jp": 3000, "bs": 0, "p_jp": 0.01, "p_bs": 0},
        "option2": {"gain": np.random.choice([8, 9], n_choices), "loss": np.random.choice([-9, -8], n_choices), "jp": 0, "bs": 0, "p_jp": 0, "p_bs": 0},
        "option3": {"gain": np.random.choice([8, 9], n_choices), "loss": np.random.choice([-4, -3], n_choices), "jp": 0, "bs": -3000, "p_jp": 0, "p_bs": 0.01},
        "option4": {"gain": np.random.choice([3, 4], n_choices), "loss": np.random.choice([-4, -3], n_choices), "jp": 3000, "bs": -3000, "p_jp": 0.01, "p_bs": 0.01},
    }

    res = simulation_ree_learning_biased_simple_rl(n_choices=n_choices, options=options, alpha1=0.5, alpha2=0.5, alpha3=0.5, alpha4=0.5, forget=0.1, lambda_=1.0, rho_BS=0.0, rho_JP=0.0)
    plot_TSREE_OSSREE(res["proportions"])

# %%
# Multi agent simulation
n_agent = 1000

full_results = []
for i in range(n_agent):
    n_choices = 400  # Nombre total de choix
    options = {
        "option1": {"gain": np.random.choice([3, 4], n_choices), "loss": np.random.choice([-9, -8], n_choices), "jp": 3000, "bs": 0, "p_jp": 0.01, "p_bs": 0},
        "option2": {"gain": np.random.choice([8, 9], n_choices), "loss": np.random.choice([-9, -8], n_choices), "jp": 0, "bs": 0, "p_jp": 0, "p_bs": 0},
        "option3": {"gain": np.random.choice([8, 9], n_choices), "loss": np.random.choice([-4, -3], n_choices), "jp": 0, "bs": -3000, "p_jp": 0, "p_bs": 0.01},
        "option4": {"gain": np.random.choice([3, 4], n_choices), "loss": np.random.choice([-4, -3], n_choices), "jp": 3000, "bs": -3000, "p_jp": 0.01, "p_bs": 0.01},
    }
    # simulate agent
    result = simulation_ree_learning_biased_simple_rl(
        n_choices=n_choices,
        options=options,
        alpha1=0.5,
        alpha2=0.5,
        alpha3=0.5,
        alpha4=0.5,
        forget=0.2,
        lambda_=2,
        rho_BS=-1,
        rho_JP=1
    )
    full_results.append(result)

# %%
# Plot mean proportions across agents
mean_proportions = pd.DataFrame({
    "Iteration": np.arange(1, n_choices + 1),
    "Antifragile": np.mean([res["proportions"]["Antifragile"].values for res in full_results], axis=0),
    "Robust": np.mean([res["proportions"]["Robust"].values for res in full_results], axis=0),
    "Fragil": np.mean([res["proportions"]["Fragil"].values for res in full_results], axis=0),
    "Vulnerable": np.mean([res["proportions"]["Vulnerable"].values for res in full_results], axis=0),
})

sd_proportions = pd.DataFrame({
    "Iteration": np.arange(1, n_choices + 1),
    "Antifragile": np.std([res["proportions"]["Antifragile"].values for res in full_results], axis=0),
    "Robust": np.std([res["proportions"]["Robust"].values for res in full_results], axis=0),
    "Fragil": np.std([res["proportions"]["Fragil"].values for res in full_results], axis=0),
    "Vulnerable": np.std([res["proportions"]["Vulnerable"].values for res in full_results], axis=0),
})
# %%
import matplotlib.pyplot as plt
plt.figure()
# Plot ribbon around mean
plt.plot(mean_proportions["Iteration"], mean_proportions["Antifragile"], label="Antifragile")
plt.plot(mean_proportions["Iteration"], mean_proportions["Robust"], label="Robust")
plt.plot(mean_proportions["Iteration"], mean_proportions["Fragil"], label="Fragil")
plt.plot(mean_proportions["Iteration"], mean_proportions["Vulnerable"], label="Vulnerable")
plt.fill_between(mean_proportions["Iteration"], mean_proportions["Antifragile"] - sd_proportions["Antifragile"], mean_proportions["Antifragile"] + sd_proportions["Antifragile"], alpha=0.05)
plt.fill_between(mean_proportions["Iteration"], mean_proportions["Robust"] - sd_proportions["Robust"], mean_proportions["Robust"] + sd_proportions["Robust"], alpha=0.05)
plt.fill_between(mean_proportions["Iteration"], mean_proportions["Fragil"] - sd_proportions["Fragil"], mean_proportions["Fragil"] + sd_proportions["Fragil"], alpha=0.05)
plt.fill_between(mean_proportions["Iteration"], mean_proportions["Vulnerable"] - sd_proportions["Vulnerable"], mean_proportions["Vulnerable"] + sd_proportions["Vulnerable"], alpha=0.05)
plt.xlabel("Iteration")
plt.ylabel("Proportion")
plt.title("Mean Proportions Across Agents")
plt.legend()
plt.show()
# %%
