# %%
# ============================================================================
# OPTIMISATION PYVBMC POUR MODÈLES Q-LEARNING AVEC ÉVÉNEMENTS RARES
# ============================================================================

import numpy as np
import pandas as pd
from scipy.special import expit  # logistic function
from scipy.optimize import minimize
import warnings
from typing import Dict, List, Tuple, Optional
import json
from pathlib import Path

# Tentative d'import PyVBMC
try:
    from pyvbmc import VBMC
    from pyvbmc.priors import UniformBox

    PYVBMC_AVAILABLE = True
except ImportError:
    PYVBMC_AVAILABLE = False
    warnings.warn("PyVBMC not installed. Install with: pip install pyvbmc")

# Import des données
from load_data import all_participant_data, unique_participants

# ============================================================================
# CONFIGURATIONS DES MODÈLES EMBOÎTÉS
# ============================================================================


# %%
def get_model_configs() -> Dict:
    """Retourne les configurations des différents modèles."""
    return {
        "HOMOGENEOUS": {
            "name": "HOMOGENEOUS",
            "n_alpha": 1,
            "n_forget": 1,
            "n_lambda": 1,
            "has_rho": False,
            "n_params": 3,
            "param_names": ["alpha", "forget", "lambda"],
            "lower": np.array([-5, -5, -3]),
            "upper": np.array([5, 5, 3]),
        },
        "GAIN_LOSS": {
            "name": "GAIN_LOSS",
            "n_alpha": 2,
            "n_forget": 1,
            "n_lambda": 1,
            "has_rho": False,
            "n_params": 4,
            "param_names": ["alpha_loss", "alpha_gain", "forget", "lambda"],
            "lower": np.array([-5, -5, -5, -3]),
            "upper": np.array([5, 5, 5, 3]),
        },
        "BIASED": {
            "name": "BIASED",
            "n_alpha": 2,
            "n_forget": 4,
            "n_lambda": 4,
            "has_rho": False,
            "n_params": 10,
            "param_names": [
                "alpha_loss",
                "alpha_gain",
                "forget_1",
                "forget_2",
                "forget_3",
                "forget_4",
                "lambda_1",
                "lambda_2",
                "lambda_3",
                "lambda_4",
            ],
            "lower": np.concatenate([[-5, -5], np.full(4, -5), np.full(4, -3)]),
            "upper": np.concatenate([[5, 5], np.full(4, 5), np.full(4, 3)]),
        },
        "REE_BIASED_SIMPLE": {
            "name": "REE_BIASED_SIMPLE",
            "n_alpha": 2,
            "n_forget": 1,
            "n_lambda": 1,
            "has_rho": True,
            "n_params": 6,
            "param_names": [
                "alpha_loss",
                "alpha_gain",
                "forget",
                "lambda",
                "rho_BS",
                "rho_JP",
            ],
            "lower": np.array([-5, -5, -5, -3, -10, -10]),
            "upper": np.array([5, 5, 5, 3, 10, 10]),
        },
        "REE_BIASED_COMPLEX": {
            "name": "REE_BIASED_COMPLEX",
            "n_alpha": 2,
            "n_forget": 4,
            "n_lambda": 4,
            "has_rho": True,
            "n_params": 12,
            "param_names": [
                "alpha_loss",
                "alpha_gain",
                "forget_1",
                "forget_2",
                "forget_3",
                "forget_4",
                "lambda_1",
                "lambda_2",
                "lambda_3",
                "lambda_4",
                "rho_BS",
                "rho_JP",
            ],
            "lower": np.concatenate(
                [[-5, -5], np.full(4, -5), np.full(4, -3), [-10, -10]]
            ),
            "upper": np.concatenate([[5, 5], np.full(4, 5), np.full(4, 3), [10, 10]]),
        },
        "REE_LEARNING_SIMPLE": {
            "name": "REE_LEARNING_SIMPLE",
            "n_alpha": 4,
            "n_forget": 1,
            "n_lambda": 1,
            "has_rho": False,
            "n_params": 6,
            "param_names": [
                "alpha_loss",
                "alpha_gain",
                "alpha_BS",
                "alpha_JP",
                "forget",
                "lambda",
            ],
            "lower": np.array([-5, -5, -5, -5, -5, -3]),
            "upper": np.array([5, 5, 5, 5, 5, 3]),
        },
        "REE_LEARNING_COMPLEX": {
            "name": "REE_LEARNING_COMPLEX",
            "n_alpha": 4,
            "n_forget": 4,
            "n_lambda": 4,
            "has_rho": False,
            "n_params": 12,
            "param_names": [
                "alpha_loss",
                "alpha_gain",
                "alpha_BS",
                "alpha_JP",
                "forget_1",
                "forget_2",
                "forget_3",
                "forget_4",
                "lambda_1",
                "lambda_2",
                "lambda_3",
                "lambda_4",
            ],
            "lower": np.concatenate([[-5, -5, -5, -5], np.full(4, -5), np.full(4, -3)]),
            "upper": np.concatenate([[5, 5, 5, 5], np.full(4, 5), np.full(4, 3)]),
        },
        "REE_LEARNING_BIASED_SIMPLE": {
            "name": "REE_LEARNING_BIASED_SIMPLE",
            "n_alpha": 4,
            "n_forget": 1,
            "n_lambda": 1,
            "has_rho": True,
            "n_params": 8,
            "param_names": [
                "alpha_loss",
                "alpha_gain",
                "alpha_BS",
                "alpha_JP",
                "forget",
                "lambda",
                "rho_BS",
                "rho_JP",
            ],
            "lower": np.array([-5, -5, -5, -5, -5, -3, -10, -10]),
            "upper": np.array([5, 5, 5, 5, 5, 3, 10, 10]),
        },
        "REE_LEARNING_BIASED_COMPLEX": {
            "name": "REE_LEARNING_BIASED_COMPLEX",
            "n_alpha": 4,
            "n_forget": 4,
            "n_lambda": 4,
            "has_rho": True,
            "n_params": 14,
            "param_names": [
                "alpha_loss",
                "alpha_gain",
                "alpha_BS",
                "alpha_JP",
                "forget_1",
                "forget_2",
                "forget_3",
                "forget_4",
                "lambda_1",
                "lambda_2",
                "lambda_3",
                "lambda_4",
                "rho_BS",
                "rho_JP",
            ],
            "lower": np.concatenate(
                [[-5, -5, -5, -5], np.full(4, -5), np.full(4, -3), [-10, -10]]
            ),
            "upper": np.concatenate(
                [[5, 5, 5, 5], np.full(4, 5), np.full(4, 3), [10, 10]]
            ),
        },
    }


# ============================================================================
# MODÈLE Q-LEARNING GÉNÉRIQUE
# ============================================================================


def qlearning_generic(
    params: np.ndarray,
    data: pd.DataFrame,
    model_config: Dict,
    return_negLL: bool = True,
) -> float:
    """
    Modèle Q-learning générique avec support pour différentes architectures de paramètres.

    Args:
        params: Vecteur de paramètres
        data: DataFrame avec colonnes 'choice', 'reward'
        model_config: Configuration du modèle
        return_negLL: Si True, retourne -log-vraisemblance; sinon retourne log-vraisemblance

    Returns:
        Valeur de la log-vraisemblance négative (ou positive selon return_negLL)
    """
    n_arms = 4
    n_trials = len(data)

    # Extraction des paramètres selon la configuration du modèle
    param_idx = 0

    # ALPHA(S)
    if model_config["n_alpha"] == 1:
        alpha_loss = alpha_gain = alpha_BS = alpha_JP = expit(params[param_idx])
        param_idx += 1
    elif model_config["n_alpha"] == 2:
        alpha_loss = expit(params[param_idx])
        alpha_gain = expit(params[param_idx + 1])
        alpha_BS = alpha_loss
        alpha_JP = alpha_gain
        param_idx += 2
    elif model_config["n_alpha"] == 4:
        alpha_loss = expit(params[param_idx])
        alpha_gain = expit(params[param_idx + 1])
        alpha_BS = expit(params[param_idx + 2])
        alpha_JP = expit(params[param_idx + 3])
        param_idx += 4

    # FORGET(S)
    if model_config["n_forget"] == 1:
        forget = np.full(n_arms, expit(params[param_idx]))
        param_idx += 1
    elif model_config["n_forget"] == 4:
        forget = expit(params[param_idx : (param_idx + 4)])
        param_idx += 4

    # LAMBDA(S)
    if model_config["n_lambda"] == 1:
        lambda_vals = np.full(n_arms, np.exp(params[param_idx]))
        param_idx += 1
    elif model_config["n_lambda"] == 4:
        lambda_vals = np.exp(params[param_idx : (param_idx + 4)])
        param_idx += 4

    # RHO(S) - Biais pour événements rares
    if model_config["has_rho"]:
        rho_BS = params[param_idx]
        rho_JP = params[param_idx + 1]
    else:
        rho_BS = rho_JP = 0

    # Initialisation des Q-values
    Q = np.zeros(n_arms)
    log_lik = 0.0

    for t in range(n_trials):
        choice = int(data.iloc[t]["choice"])
        reward = data.iloc[t]["reward"]

        # Calcul des valeurs subjectives V(t)
        V = lambda_vals * Q

        # Ajout des biais pour événements rares si le modèle le permet
        if model_config["has_rho"]:
            V[0] += rho_JP  # antifragile
            V[1] += rho_BS  # fragile
            V[3] += rho_BS + rho_JP  # vulnerable

        # Softmax
        V_max = np.max(V)
        exp_V = np.exp(V - V_max)
        probs = exp_V / np.sum(exp_V)
        probs = np.maximum(probs, 1e-10)
        probs = probs / np.sum(probs)

        # Log-likelihood
        log_lik += np.log(probs[choice])

        # Mise à jour Q-learning
        Q_new = Q.copy()

        # Choix de l'alpha approprié
        if reward == -3000:
            alpha_used = alpha_BS
        elif reward == 3000:
            alpha_used = alpha_JP
        elif reward < 0:
            alpha_used = alpha_loss
        else:
            alpha_used = alpha_gain

        # Option choisie : Q(t+1) = Q(t) + alpha * (r(t) - Q(t))
        Q_new[choice] = Q[choice] + alpha_used * (reward - Q[choice])

        # Options non choisies : Q(t+1) = Q(t) * (1 - f)
        not_chosen = np.setdiff1d(np.arange(n_arms), [choice])
        Q_new[not_chosen] = Q[not_chosen] * (1 - forget[not_chosen])

        Q = Q_new

    if return_negLL:
        return -log_lik
    else:
        return log_lik


# %%
# ============================================================================
# OPTIMISATION AVEC PYVBMC
# ============================================================================


def fit_participant_pyvbmc(
    participant_data: pd.DataFrame, model_config: Dict, verbose: bool = True, plot : bool = True
) -> Dict:
    """
    Optimise les paramètres du modèle pour un participant utilisant PyVBMC.

    Args:
        participant_data: Données du participant
        model_config: Configuration du modèle
        verbose: Affiche les progressions

    Returns:
        Dictionnaire avec les résultats d'optimisation
    """
    if not PYVBMC_AVAILABLE:
        raise RuntimeError(
            "PyVBMC n'est pas installé. Installez avec: pip install pyvbmc"
        )

    # Définition de la fonction de log-densité pour PyVBMC
    def log_likelihood(params_array):
        """PyVBMC maximise, donc on retourne -negLL."""
        params = np.asarray(params_array).flatten()
        negLL = qlearning_generic(
            params, participant_data, model_config, return_negLL=True
        )
        return -negLL

    # Point de départ (milieu des bornes)
    x0 = (model_config["lower"] + model_config["upper"]) / 2

    # Bornes plausibles (25%-75% de la plage)
    plb = model_config["lower"] + 0.25 * (model_config["upper"] - model_config["lower"])
    pub = model_config["upper"] - 0.25 * (model_config["upper"] - model_config["lower"])

    if verbose:
        print(f"  Starting VBMC optimization...")
        print(f"    Initial parameters: {x0}")
        print(f"    Lower bounds: {model_config['lower']}")
        print(f"    Upper bounds: {model_config['upper']}")

    # Initialisation et optimisation de VBMC
    vbmc = VBMC(
        log_likelihood,
        x0,
        model_config["lower"],
        model_config["upper"],
        plb,
        pub,
        options={
            # "verbose": 0 if not verbose else 1,
            "display": "off",
            "plot": plot,
            "log_file_name": None,
        },
        prior=UniformBox(
            a=model_config["lower"], b=model_config["upper"], D=model_config["n_params"]
        ),
    )

    vp, results = vbmc.optimize()

    # Extraction des statistiques
    posterior_mean, posterior_cov = vp.moments(orig_flag=True, cov_flag=True)
    posterior_mean = np.asarray(posterior_mean).flatten()
    posterior_sd = np.sqrt(np.diag(posterior_cov))

    # ELBO et autres métriques
    elbo = results["elbo"]
    elbo_sd = results.get("elbo_sd", np.nan)
    n_iterations = results.get("iterations", np.nan)

    # Calcul du negLL avec la posterior mean
    negLL = qlearning_generic(
        posterior_mean, participant_data, model_config, return_negLL=True
    )
    n_obs = len(participant_data)

    # Calcul des critères d'information
    aic = 2 * negLL + 2 * model_config["n_params"]
    bic = 2 * negLL + model_config["n_params"] * np.log(n_obs)

    result = {
        "model": model_config["name"],
        "n_params": model_config["n_params"],
        "negLL": negLL,
        "AIC": aic,
        "BIC": bic,
        "ELBO": elbo,
        "ELBO_SD": elbo_sd,
        "n_iterations": n_iterations,
        "converged": True,
        "method": "VBMC",
        "posterior_mean": posterior_mean,
        "posterior_sd": posterior_sd,
        "vp": vp,
        "results": results,
    }

    # Ajout des paramètres estimés
    for i, param_name in enumerate(model_config["param_names"]):
        # Here we use expit for parameters that were originally bounded between 0 and 1
        if param_name.startswith("alpha") or param_name.startswith("forget"):
            result[param_name] = expit(posterior_mean[i])
            result[f"sd_{param_name}"] = expit(posterior_sd[i])
        else:
            result[param_name] = posterior_mean[i]
            result[f"sd_{param_name}"] = posterior_sd[i]

    return result


def fit_participant_deoptim(
    participant_data: pd.DataFrame,
    model_config: Dict,
    n_runs: int = 5,
    verbose: bool = True,
    n_workers: int = 1,
) -> Dict:
    """
    Optimise les paramètres du modèle pour un participant utilisant minimisation scipy.

    Args:
        participant_data: Données du participant
        model_config: Configuration du modèle
        n_runs: Nombre de runs avec différents points de départ
        verbose: Affiche les progressions

    Returns:
        Dictionnaire avec les résultats d'optimisation
    """
    from scipy.optimize import differential_evolution

    best_result = None
    best_negLL = np.inf
    all_negLLs = []

    if verbose:
        print(f"  Running {n_runs} optimization runs...")

    for run in range(n_runs):
        np.random.seed(1000 * hash(model_config["name"]) % (2**31) + run)

        def objective(params):
            return qlearning_generic(
                params, participant_data, model_config, return_negLL=True
            )

        result = differential_evolution(
            objective,
            bounds=list(zip(model_config["lower"], model_config["upper"])),
            maxiter=200,
            popsize=max(50, model_config["n_params"] * 10),
            rng=1000 * hash(model_config["name"]) % (2**31) + run,
            workers=n_workers,
            updating="deferred",
        )

        all_negLLs.append(result.fun)

        if result.fun < best_negLL:
            best_negLL = result.fun
            best_result = result

    posterior_mean = best_result.x
    negLL = best_negLL
    n_obs = len(participant_data)

    # Calcul des critères d'information
    aic = 2 * negLL + 2 * model_config["n_params"]
    bic = 2 * negLL + model_config["n_params"] * np.log(n_obs)

    # Statistiques de convergence
    convergence_sd = np.std(all_negLLs)
    convergence_range = np.max(all_negLLs) - np.min(all_negLLs)

    result_dict = {
        "model": model_config["name"],
        "n_params": model_config["n_params"],
        "negLL": negLL,
        "AIC": aic,
        "BIC": bic,
        "n_runs": n_runs,
        "convergence_sd": convergence_sd,
        "convergence_range": convergence_range,
        "converged": convergence_range < 1,
        "method": "Differential Evolution",
        "posterior_mean": posterior_mean,
    }

    # Ajout des paramètres estimés après les avoir renvoyés dans par logis
    for i, param_name in enumerate(model_config["param_names"]):
        # Here we expit alpha, forget
        if param_name.startswith("alpha") or param_name.startswith("forget"):
            result_dict[param_name] = expit(posterior_mean[i])
        else:
            result_dict[param_name] = posterior_mean[i]

    return result_dict


# ============================================================================
# OPTIMISATION POUR TOUS LES PARTICIPANTS ET MODÈLES
# ============================================================================


def fit_all_participants(
    data: pd.DataFrame,
    models_to_fit: Optional[List[str]] = None,
    method: str = "VBMC",
    n_participants: Optional[int] = None,
    verbose: bool = True,
    plot: bool = True,
) -> Dict[str, List[Dict]]:
    """
    Ajuste tous les modèles pour tous les participants.

    Args:
        data: DataFrame avec les données de tous les participants
        models_to_fit: Liste des noms de modèles à ajuster (None = tous)
        method: Méthode d'optimisation ("VBMC" ou "differential_evolution")
        n_participants: Nombre de participants à traiter (None = tous)
        verbose: Affiche les progressions

    Returns:
        Dictionnaire avec les résultats par modèle
    """
    model_configs = get_model_configs()

    if models_to_fit is not None:
        model_configs = {k: v for k, v in model_configs.items() if k in models_to_fit}

    participants = data["participant"].unique()
    if n_participants is not None:
        participants = participants[:n_participants]

    all_results = {}

    for model_name, model_config in model_configs.items():
        if verbose:
            print(f"\n=== Fitting model: {model_name} ===")

        model_results = []

        for participant_id in participants:
            if verbose:
                print(f"  Participant: {participant_id}")

            participant_data = data[data["participant"] == participant_id].copy()

            try:
                if method == "VBMC":
                    result = fit_participant_pyvbmc(
                        participant_data, model_config, verbose=False, plot=plot
                    )
                else:
                    result = fit_participant_deoptim(
                        participant_data, model_config, n_runs=5, verbose=False
                    )

                result["participant"] = participant_id
                model_results.append(result)

                if verbose:
                    print(f"    negLL: {result['negLL']:.2f}, BIC: {result['BIC']:.2f}")

            except Exception as e:
                print(f"    ERROR: {str(e)}")
                continue

        all_results[model_name] = model_results

    return all_results


# ============================================================================
# COMPARAISON DES MODÈLES
# ============================================================================


def compare_models(all_results: Dict[str, List[Dict]]) -> Dict:
    """
    Compare les modèles et sélectionne les meilleurs par participant.

    Args:
        all_results: Résultats de l'ajustement de tous les modèles

    Returns:
        Dictionnaire avec comparaisons globales et par participant
    """
    # Comparaison globale
    global_comparison = []

    for model_name, results in all_results.items():
        if len(results) == 0:
            continue

        results_df = pd.DataFrame(results)

        comparison_row = {
            "model": model_name,
            "n_params": results[0]["n_params"],
            "n_converged": sum([1 for r in results if r["converged"]]),
            "n_participants": len(results),
            "mean_negLL": results_df["negLL"].mean(),
            "total_negLL": results_df["negLL"].sum(),
            "total_AIC": results_df["AIC"].sum(),
            "total_BIC": results_df["BIC"].sum(),
        }
        global_comparison.append(comparison_row)

    global_comparison_df = pd.DataFrame(global_comparison).sort_values("total_BIC")

    print("\n=== GLOBAL MODEL COMPARISON ===")
    print(global_comparison_df.to_string(index=False))

    # Meilleur modèle par participant
    all_results_list = []
    for model_name, results in all_results.items():
        for result in results:
            all_results_list.append(
                {
                    "participant": result["participant"],
                    "model": model_name,
                    "BIC": result["BIC"],
                    "AIC": result["AIC"],
                    "negLL": result["negLL"],
                }
            )

    all_results_df = pd.DataFrame(all_results_list)
    best_per_participant = all_results_df.loc[
        all_results_df.groupby("participant")["BIC"].idxmin()
    ]

    print("\n=== BEST MODELS PER PARTICIPANT ===")
    print(best_per_participant["model"].value_counts())

    return {
        "global_comparison": global_comparison_df,
        "best_per_participant": best_per_participant,
        "all_results": all_results,
    }


# ============================================================================
# SAUVEGARDE DES RÉSULTATS
# ============================================================================


def save_results(
    all_results: Dict[str, List[Dict]], output_dir: str = "results"
) -> None:
    """
    Sauvegarde les résultats d'optimisation en CSV.

    Args:
        all_results: Résultats de l'ajustement
        output_dir: Répertoire de sortie
    """
    output_path = Path(output_dir)
    output_path.mkdir(exist_ok=True)

    for model_name, results in all_results.items():
        results_df = pd.DataFrame(results)

        # Garder seulement les colonnes numériques pour le CSV
        cols_to_keep = [
            col
            for col in results_df.columns
            if col not in ["vp", "results", "posterior_mean", "posterior_sd"]
        ]
        results_df[cols_to_keep].to_csv(
            output_path / f"results_{model_name}.csv", index=False
        )
        print(f"Saved: results_{model_name}.csv")


def fit_vbmc_and_diffEvol(
    participant_data: pd.DataFrame,
    model_config: Dict,
    n_deoptim_runs: int = 5,
    n_workers: int = 1,
    verbose: bool = True,
) -> Tuple[Dict, Dict]:
    """
    Ajuste un modèle à l'aide de PyVBMC et Differential Evolution pour comparaison.

    Args:
        participant_data: Données du participant
        model_config: Configuration du modèle
        n_deoptim_runs: Nombre de runs pour Differential Evolution
        verbose: Affiche les progressions

    Returns:
        Tuple avec les résultats VBMC et Differential Evolution
    """
    if verbose:
        print(f"  Fitting with VBMC")
    vbmc_result = fit_participant_pyvbmc(
        participant_data, model_config, verbose=verbose
    )
    if verbose:
        print(f"  Fitting with Differential Evolution")
    deoptim_result = fit_participant_deoptim(
        participant_data,
        model_config,
        n_runs=n_deoptim_runs,
        n_workers=n_workers,
        verbose=verbose,
    )
    return vbmc_result, deoptim_result


def fit_all_participants_both_methods(
    data: pd.DataFrame,
    models_to_fit: Optional[List[str]] = None,
    n_participants: Optional[int] = None,
    n_deoptim_runs: int = 5,
    n_workers: int = 1,
    verbose: bool = True,
) -> Dict[str, List[Dict]]:
    """
    Ajuste tous les modèles pour tous les participants avec les deux méthodes.

    Args:
        data: DataFrame avec les données de tous les participants
        models_to_fit: Liste des noms de modèles à ajuster (None = tous)
        n_participants: Nombre de participants à traiter (None = tous)
        n_deoptim_runs: Nombre de runs pour Differential Evolution
        verbose: Affiche les progressions

    Returns:
        Dictionnaire avec les résultats par modèle et méthode
    """
    model_configs = get_model_configs()

    if models_to_fit is not None:
        model_configs = {k: v for k, v in model_configs.items() if k in models_to_fit}

    participants = data["participant"].unique()
    if n_participants is not None:
        participants = participants[:n_participants]

    all_results = {}

    for model_name, model_config in model_configs.items():
        if verbose:
            print(f"\n=== Fitting model: {model_name} ===")

        model_results = []

        for participant_id in participants:
            if verbose:
                print(f"  Participant: {participant_id}")

            participant_data = data[data["participant"] == participant_id].copy()

            try:
                vbmc_result, deoptim_result = fit_vbmc_and_diffEvol(
                    participant_data,
                    model_config,
                    n_deoptim_runs=n_deoptim_runs,
                    n_workers=n_workers,
                    verbose=False,
                )

                vbmc_result["participant"] = participant_id
                deoptim_result["participant"] = participant_id

                model_results.append(
                    {"VBMC": vbmc_result, "Differential_Evolution": deoptim_result}
                )

                if verbose:
                    print(
                        f"    VBMC negLL: {vbmc_result['negLL']:.2f}, BIC: {vbmc_result['BIC']:.2f}"
                    )
                    print(
                        f"    DE negLL: {deoptim_result['negLL']:.2f}, BIC: {deoptim_result['BIC']:.2f}"
                    )

            except Exception as e:
                print(f"    ERROR: {str(e)}")
                continue

        all_results[model_name] = model_results
    return all_results


# %%
# ============================================================================
# EXEMPLE D'UTILISATION
# ============================================================================

if __name__ == "__main__":
    print("===  Optimization for Q-Learning Models ===\n")

    # Préparation des données
    print("Loading data...")
    data_for_fitting = all_participant_data[["participant", "choice", "reward"]].copy()
    print(f"  Total participants: {data_for_fitting['participant'].nunique()}")
    print(f"  Total trials: {len(data_for_fitting)}")

    # Ajustement des modèles
    method = "differential_evolution"  # "VBMC" ou "differential_evolution"
    if PYVBMC_AVAILABLE:
        method = "VBMC"
        print(f"  PyVBMC available - using {method}")
    else:
        print(f"  PyVBMC not available - using {method}")

    # Ajustement de quelques modèles pour test
    models_to_fit = [
        # "HOMOGENEOUS",
        # "GAIN_LOSS",
        # "REE_BIASED_SIMPLE",
        # "REE_BIASED_COMPLEX",
        # "REE_LEARNING_SIMPLE",
        # "REE_LEARNING_COMPLEX",
        "REE_LEARNING_BIASED_SIMPLE",
        # "REE_LEARNING_BIASED_COMPLEX",
    ]

    # all_results = fit_all_participants_both_methods(
    #     data_for_fitting,
    #     models_to_fit=models_to_fit,
    #     # method=method,
    #     n_participants=2,  # Set to a number to limit for testing
    #     n_workers=1,
    #     verbose=True,
    # )

    all_results = fit_all_participants(
        data_for_fitting,
        models_to_fit=models_to_fit,
        method=method,
        n_participants=1,  # Set to a number to limit for testing
        verbose=True,
        plot=True
    )

    # Comparaison des modèles
    comparison = compare_models(all_results)

    # Sauvegarde des résultats
    print("\nSaving results...")
    save_results(all_results)
    comparison["global_comparison"].to_csv("results/global_comparison.csv", index=False)
    comparison["best_per_participant"].to_csv("results/best_models.csv", index=False)

    print("\nDone!")

# %%
