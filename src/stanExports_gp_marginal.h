// Generated by rstantools.  Do not edit by hand.

/*
    bmstdr is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    bmstdr is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with bmstdr.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#include <rstan/rstaninc.hpp>
// Code generated by Stan version 2.21.0
#include <stan/model/model_header.hpp>
namespace model_gp_marginal_namespace {
using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::prob_grad;
using namespace stan::math;
static int current_statement_begin__;
stan::io::program_reader prog_reader__() {
    stan::io::program_reader reader;
    reader.add_event(0, 0, "start", "model_gp_marginal");
    reader.add_event(106, 104, "end", "model_gp_marginal");
    return reader;
}
#include <stan_meta_header.hpp>
class model_gp_marginal
  : public stan::model::model_base_crtp<model_gp_marginal> {
private:
        int sn;
        int tn;
        int nT;
        int p;
        int ntmiss;
        int ntobs;
        int missing;
        std::vector<int> data_miss_idx;
        std::vector<int> data_obs_idx;
        std::vector<double> yobs;
        matrix_d X;
        std::vector<double> sigma2_prior;
        std::vector<double> tau2_prior;
        int phidist;
        std::vector<double> prior_phi_param;
        matrix_d dist;
        int verbose;
        vector_d mu_0;
public:
    model_gp_marginal(stan::io::var_context& context__,
        std::ostream* pstream__ = 0)
        : model_base_crtp(0) {
        ctor_body(context__, 0, pstream__);
    }
    model_gp_marginal(stan::io::var_context& context__,
        unsigned int random_seed__,
        std::ostream* pstream__ = 0)
        : model_base_crtp(0) {
        ctor_body(context__, random_seed__, pstream__);
    }
    void ctor_body(stan::io::var_context& context__,
                   unsigned int random_seed__,
                   std::ostream* pstream__) {
        typedef double local_scalar_t__;
        boost::ecuyer1988 base_rng__ =
          stan::services::util::create_rng(random_seed__, 0);
        (void) base_rng__;  // suppress unused var warning
        current_statement_begin__ = -1;
        static const char* function__ = "model_gp_marginal_namespace::model_gp_marginal";
        (void) function__;  // dummy to suppress unused var warning
        size_t pos__;
        (void) pos__;  // dummy to suppress unused var warning
        std::vector<int> vals_i__;
        std::vector<double> vals_r__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        try {
            // initialize data block variables from context__
            current_statement_begin__ = 13;
            context__.validate_dims("data initialization", "sn", "int", context__.to_vec());
            sn = int(0);
            vals_i__ = context__.vals_i("sn");
            pos__ = 0;
            sn = vals_i__[pos__++];
            check_greater_or_equal(function__, "sn", sn, 0);
            current_statement_begin__ = 14;
            context__.validate_dims("data initialization", "tn", "int", context__.to_vec());
            tn = int(0);
            vals_i__ = context__.vals_i("tn");
            pos__ = 0;
            tn = vals_i__[pos__++];
            check_greater_or_equal(function__, "tn", tn, 0);
            current_statement_begin__ = 15;
            context__.validate_dims("data initialization", "nT", "int", context__.to_vec());
            nT = int(0);
            vals_i__ = context__.vals_i("nT");
            pos__ = 0;
            nT = vals_i__[pos__++];
            check_greater_or_equal(function__, "nT", nT, 0);
            current_statement_begin__ = 16;
            context__.validate_dims("data initialization", "p", "int", context__.to_vec());
            p = int(0);
            vals_i__ = context__.vals_i("p");
            pos__ = 0;
            p = vals_i__[pos__++];
            check_greater_or_equal(function__, "p", p, 0);
            current_statement_begin__ = 17;
            context__.validate_dims("data initialization", "ntmiss", "int", context__.to_vec());
            ntmiss = int(0);
            vals_i__ = context__.vals_i("ntmiss");
            pos__ = 0;
            ntmiss = vals_i__[pos__++];
            check_greater_or_equal(function__, "ntmiss", ntmiss, 0);
            current_statement_begin__ = 18;
            context__.validate_dims("data initialization", "ntobs", "int", context__.to_vec());
            ntobs = int(0);
            vals_i__ = context__.vals_i("ntobs");
            pos__ = 0;
            ntobs = vals_i__[pos__++];
            check_greater_or_equal(function__, "ntobs", ntobs, 0);
            current_statement_begin__ = 19;
            context__.validate_dims("data initialization", "missing", "int", context__.to_vec());
            missing = int(0);
            vals_i__ = context__.vals_i("missing");
            pos__ = 0;
            missing = vals_i__[pos__++];
            check_greater_or_equal(function__, "missing", missing, 0);
            check_less_or_equal(function__, "missing", missing, 1);
            current_statement_begin__ = 20;
            validate_non_negative_index("data_miss_idx", "ntmiss", ntmiss);
            context__.validate_dims("data initialization", "data_miss_idx", "int", context__.to_vec(ntmiss));
            data_miss_idx = std::vector<int>(ntmiss, int(0));
            vals_i__ = context__.vals_i("data_miss_idx");
            pos__ = 0;
            size_t data_miss_idx_k_0_max__ = ntmiss;
            for (size_t k_0__ = 0; k_0__ < data_miss_idx_k_0_max__; ++k_0__) {
                data_miss_idx[k_0__] = vals_i__[pos__++];
            }
            current_statement_begin__ = 21;
            validate_non_negative_index("data_obs_idx", "ntobs", ntobs);
            context__.validate_dims("data initialization", "data_obs_idx", "int", context__.to_vec(ntobs));
            data_obs_idx = std::vector<int>(ntobs, int(0));
            vals_i__ = context__.vals_i("data_obs_idx");
            pos__ = 0;
            size_t data_obs_idx_k_0_max__ = ntobs;
            for (size_t k_0__ = 0; k_0__ < data_obs_idx_k_0_max__; ++k_0__) {
                data_obs_idx[k_0__] = vals_i__[pos__++];
            }
            current_statement_begin__ = 22;
            validate_non_negative_index("yobs", "ntobs", ntobs);
            context__.validate_dims("data initialization", "yobs", "double", context__.to_vec(ntobs));
            yobs = std::vector<double>(ntobs, double(0));
            vals_r__ = context__.vals_r("yobs");
            pos__ = 0;
            size_t yobs_k_0_max__ = ntobs;
            for (size_t k_0__ = 0; k_0__ < yobs_k_0_max__; ++k_0__) {
                yobs[k_0__] = vals_r__[pos__++];
            }
            current_statement_begin__ = 23;
            validate_non_negative_index("X", "nT", nT);
            validate_non_negative_index("X", "p", p);
            context__.validate_dims("data initialization", "X", "matrix_d", context__.to_vec(nT,p));
            X = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>(nT, p);
            vals_r__ = context__.vals_r("X");
            pos__ = 0;
            size_t X_j_2_max__ = p;
            size_t X_j_1_max__ = nT;
            for (size_t j_2__ = 0; j_2__ < X_j_2_max__; ++j_2__) {
                for (size_t j_1__ = 0; j_1__ < X_j_1_max__; ++j_1__) {
                    X(j_1__, j_2__) = vals_r__[pos__++];
                }
            }
            current_statement_begin__ = 24;
            validate_non_negative_index("sigma2_prior", "2", 2);
            context__.validate_dims("data initialization", "sigma2_prior", "double", context__.to_vec(2));
            sigma2_prior = std::vector<double>(2, double(0));
            vals_r__ = context__.vals_r("sigma2_prior");
            pos__ = 0;
            size_t sigma2_prior_k_0_max__ = 2;
            for (size_t k_0__ = 0; k_0__ < sigma2_prior_k_0_max__; ++k_0__) {
                sigma2_prior[k_0__] = vals_r__[pos__++];
            }
            size_t sigma2_prior_i_0_max__ = 2;
            for (size_t i_0__ = 0; i_0__ < sigma2_prior_i_0_max__; ++i_0__) {
                check_greater_or_equal(function__, "sigma2_prior[i_0__]", sigma2_prior[i_0__], 0);
            }
            current_statement_begin__ = 25;
            validate_non_negative_index("tau2_prior", "2", 2);
            context__.validate_dims("data initialization", "tau2_prior", "double", context__.to_vec(2));
            tau2_prior = std::vector<double>(2, double(0));
            vals_r__ = context__.vals_r("tau2_prior");
            pos__ = 0;
            size_t tau2_prior_k_0_max__ = 2;
            for (size_t k_0__ = 0; k_0__ < tau2_prior_k_0_max__; ++k_0__) {
                tau2_prior[k_0__] = vals_r__[pos__++];
            }
            size_t tau2_prior_i_0_max__ = 2;
            for (size_t i_0__ = 0; i_0__ < tau2_prior_i_0_max__; ++i_0__) {
                check_greater_or_equal(function__, "tau2_prior[i_0__]", tau2_prior[i_0__], 0);
            }
            current_statement_begin__ = 26;
            context__.validate_dims("data initialization", "phidist", "int", context__.to_vec());
            phidist = int(0);
            vals_i__ = context__.vals_i("phidist");
            pos__ = 0;
            phidist = vals_i__[pos__++];
            check_greater_or_equal(function__, "phidist", phidist, 0);
            current_statement_begin__ = 27;
            validate_non_negative_index("prior_phi_param", "2", 2);
            context__.validate_dims("data initialization", "prior_phi_param", "double", context__.to_vec(2));
            prior_phi_param = std::vector<double>(2, double(0));
            vals_r__ = context__.vals_r("prior_phi_param");
            pos__ = 0;
            size_t prior_phi_param_k_0_max__ = 2;
            for (size_t k_0__ = 0; k_0__ < prior_phi_param_k_0_max__; ++k_0__) {
                prior_phi_param[k_0__] = vals_r__[pos__++];
            }
            size_t prior_phi_param_i_0_max__ = 2;
            for (size_t i_0__ = 0; i_0__ < prior_phi_param_i_0_max__; ++i_0__) {
                check_greater_or_equal(function__, "prior_phi_param[i_0__]", prior_phi_param[i_0__], 0);
            }
            current_statement_begin__ = 28;
            validate_non_negative_index("dist", "sn", sn);
            validate_non_negative_index("dist", "sn", sn);
            context__.validate_dims("data initialization", "dist", "matrix_d", context__.to_vec(sn,sn));
            dist = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>(sn, sn);
            vals_r__ = context__.vals_r("dist");
            pos__ = 0;
            size_t dist_j_2_max__ = sn;
            size_t dist_j_1_max__ = sn;
            for (size_t j_2__ = 0; j_2__ < dist_j_2_max__; ++j_2__) {
                for (size_t j_1__ = 0; j_1__ < dist_j_1_max__; ++j_1__) {
                    dist(j_1__, j_2__) = vals_r__[pos__++];
                }
            }
            current_statement_begin__ = 29;
            context__.validate_dims("data initialization", "verbose", "int", context__.to_vec());
            verbose = int(0);
            vals_i__ = context__.vals_i("verbose");
            pos__ = 0;
            verbose = vals_i__[pos__++];
            check_greater_or_equal(function__, "verbose", verbose, 0);
            // initialize transformed data variables
            current_statement_begin__ = 33;
            validate_non_negative_index("mu_0", "sn", sn);
            mu_0 = Eigen::Matrix<double, Eigen::Dynamic, 1>(sn);
            stan::math::fill(mu_0, DUMMY_VAR__);
            stan::math::assign(mu_0,rep_vector(0, sn));
            // execute transformed data statements
            // validate transformed data
            // validate, set parameter ranges
            num_params_r__ = 0U;
            param_ranges_i__.clear();
            current_statement_begin__ = 37;
            validate_non_negative_index("beta", "p", p);
            num_params_r__ += p;
            current_statement_begin__ = 38;
            num_params_r__ += 1;
            current_statement_begin__ = 39;
            num_params_r__ += 1;
            current_statement_begin__ = 40;
            num_params_r__ += 1;
            current_statement_begin__ = 41;
            validate_non_negative_index("z_miss", "(missing ? ntmiss : 0 )", (missing ? ntmiss : 0 ));
            num_params_r__ += (1 * (missing ? ntmiss : 0 ));
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }
    ~model_gp_marginal() { }
    void transform_inits(const stan::io::var_context& context__,
                         std::vector<int>& params_i__,
                         std::vector<double>& params_r__,
                         std::ostream* pstream__) const {
        typedef double local_scalar_t__;
        stan::io::writer<double> writer__(params_r__, params_i__);
        size_t pos__;
        (void) pos__; // dummy call to supress warning
        std::vector<double> vals_r__;
        std::vector<int> vals_i__;
        current_statement_begin__ = 37;
        if (!(context__.contains_r("beta")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable beta missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("beta");
        pos__ = 0U;
        validate_non_negative_index("beta", "p", p);
        context__.validate_dims("parameter initialization", "beta", "vector_d", context__.to_vec(p));
        Eigen::Matrix<double, Eigen::Dynamic, 1> beta(p);
        size_t beta_j_1_max__ = p;
        for (size_t j_1__ = 0; j_1__ < beta_j_1_max__; ++j_1__) {
            beta(j_1__) = vals_r__[pos__++];
        }
        try {
            writer__.vector_unconstrain(beta);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable beta: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 38;
        if (!(context__.contains_r("phi")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable phi missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("phi");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "phi", "double", context__.to_vec());
        double phi(0);
        phi = vals_r__[pos__++];
        try {
            writer__.scalar_lb_unconstrain(0, phi);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable phi: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 39;
        if (!(context__.contains_r("sigma_sq")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable sigma_sq missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("sigma_sq");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "sigma_sq", "double", context__.to_vec());
        double sigma_sq(0);
        sigma_sq = vals_r__[pos__++];
        try {
            writer__.scalar_lb_unconstrain(0, sigma_sq);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable sigma_sq: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 40;
        if (!(context__.contains_r("tau_sq")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable tau_sq missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("tau_sq");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "tau_sq", "double", context__.to_vec());
        double tau_sq(0);
        tau_sq = vals_r__[pos__++];
        try {
            writer__.scalar_lb_unconstrain(0, tau_sq);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable tau_sq: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 41;
        if (!(context__.contains_r("z_miss")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable z_miss missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("z_miss");
        pos__ = 0U;
        validate_non_negative_index("z_miss", "(missing ? ntmiss : 0 )", (missing ? ntmiss : 0 ));
        context__.validate_dims("parameter initialization", "z_miss", "double", context__.to_vec((missing ? ntmiss : 0 )));
        std::vector<double> z_miss((missing ? ntmiss : 0 ), double(0));
        size_t z_miss_k_0_max__ = (missing ? ntmiss : 0 );
        for (size_t k_0__ = 0; k_0__ < z_miss_k_0_max__; ++k_0__) {
            z_miss[k_0__] = vals_r__[pos__++];
        }
        size_t z_miss_i_0_max__ = (missing ? ntmiss : 0 );
        for (size_t i_0__ = 0; i_0__ < z_miss_i_0_max__; ++i_0__) {
            try {
                writer__.scalar_unconstrain(z_miss[i_0__]);
            } catch (const std::exception& e) {
                stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable z_miss: ") + e.what()), current_statement_begin__, prog_reader__());
            }
        }
        params_r__ = writer__.data_r();
        params_i__ = writer__.data_i();
    }
    void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double, Eigen::Dynamic, 1>& params_r,
                         std::ostream* pstream__) const {
      std::vector<double> params_r_vec;
      std::vector<int> params_i_vec;
      transform_inits(context, params_i_vec, params_r_vec, pstream__);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r(i) = params_r_vec[i];
    }
    template <bool propto__, bool jacobian__, typename T__>
    T__ log_prob(std::vector<T__>& params_r__,
                 std::vector<int>& params_i__,
                 std::ostream* pstream__ = 0) const {
        typedef T__ local_scalar_t__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // dummy to suppress unused var warning
        T__ lp__(0.0);
        stan::math::accumulator<T__> lp_accum__;
        try {
            stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
            // model parameters
            current_statement_begin__ = 37;
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, 1> beta;
            (void) beta;  // dummy to suppress unused var warning
            if (jacobian__)
                beta = in__.vector_constrain(p, lp__);
            else
                beta = in__.vector_constrain(p);
            current_statement_begin__ = 38;
            local_scalar_t__ phi;
            (void) phi;  // dummy to suppress unused var warning
            if (jacobian__)
                phi = in__.scalar_lb_constrain(0, lp__);
            else
                phi = in__.scalar_lb_constrain(0);
            current_statement_begin__ = 39;
            local_scalar_t__ sigma_sq;
            (void) sigma_sq;  // dummy to suppress unused var warning
            if (jacobian__)
                sigma_sq = in__.scalar_lb_constrain(0, lp__);
            else
                sigma_sq = in__.scalar_lb_constrain(0);
            current_statement_begin__ = 40;
            local_scalar_t__ tau_sq;
            (void) tau_sq;  // dummy to suppress unused var warning
            if (jacobian__)
                tau_sq = in__.scalar_lb_constrain(0, lp__);
            else
                tau_sq = in__.scalar_lb_constrain(0);
            current_statement_begin__ = 41;
            std::vector<local_scalar_t__> z_miss;
            size_t z_miss_d_0_max__ = (missing ? ntmiss : 0 );
            z_miss.reserve(z_miss_d_0_max__);
            for (size_t d_0__ = 0; d_0__ < z_miss_d_0_max__; ++d_0__) {
                if (jacobian__)
                    z_miss.push_back(in__.scalar_constrain(lp__));
                else
                    z_miss.push_back(in__.scalar_constrain());
            }
            // model body
            {
            current_statement_begin__ = 46;
            validate_non_negative_index("xbmodel", "nT", nT);
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, 1> xbmodel(nT);
            stan::math::initialize(xbmodel, DUMMY_VAR__);
            stan::math::fill(xbmodel, DUMMY_VAR__);
            current_statement_begin__ = 47;
            validate_non_negative_index("mus", "tn", tn);
            validate_non_negative_index("mus", "sn", sn);
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, Eigen::Dynamic> mus(tn, sn);
            stan::math::initialize(mus, DUMMY_VAR__);
            stan::math::fill(mus, DUMMY_VAR__);
            current_statement_begin__ = 48;
            validate_non_negative_index("dats", "tn", tn);
            validate_non_negative_index("dats", "sn", sn);
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, Eigen::Dynamic> dats(tn, sn);
            stan::math::initialize(dats, DUMMY_VAR__);
            stan::math::fill(dats, DUMMY_VAR__);
            current_statement_begin__ = 49;
            validate_non_negative_index("z1", "nT", nT);
            std::vector<local_scalar_t__  > z1(nT, local_scalar_t__(DUMMY_VAR__));
            stan::math::initialize(z1, DUMMY_VAR__);
            stan::math::fill(z1, DUMMY_VAR__);
            current_statement_begin__ = 50;
            validate_non_negative_index("L", "sn", sn);
            validate_non_negative_index("L", "sn", sn);
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, Eigen::Dynamic> L(sn, sn);
            stan::math::initialize(L, DUMMY_VAR__);
            stan::math::fill(L, DUMMY_VAR__);
            current_statement_begin__ = 51;
            validate_non_negative_index("Sigma", "sn", sn);
            validate_non_negative_index("Sigma", "sn", sn);
            Eigen::Matrix<local_scalar_t__, Eigen::Dynamic, Eigen::Dynamic> Sigma(sn, sn);
            stan::math::initialize(Sigma, DUMMY_VAR__);
            stan::math::fill(Sigma, DUMMY_VAR__);
            current_statement_begin__ = 52;
            local_scalar_t__ u(DUMMY_VAR__);
            (void) u;  // dummy to suppress unused var warning
            stan::math::initialize(u, DUMMY_VAR__);
            stan::math::fill(u, DUMMY_VAR__);
            current_statement_begin__ = 55;
            lp_accum__.add(inv_gamma_log<propto__>(sigma_sq, get_base1(sigma2_prior, 1, "sigma2_prior", 1), get_base1(sigma2_prior, 2, "sigma2_prior", 1)));
            current_statement_begin__ = 56;
            lp_accum__.add(inv_gamma_log<propto__>(tau_sq, get_base1(tau2_prior, 1, "tau2_prior", 1), get_base1(tau2_prior, 2, "tau2_prior", 1)));
            current_statement_begin__ = 57;
            if (as_bool(logical_eq(phidist, 0))) {
                current_statement_begin__ = 57;
                lp_accum__.add(uniform_log<propto__>(phi, get_base1(prior_phi_param, 1, "prior_phi_param", 1), get_base1(prior_phi_param, 2, "prior_phi_param", 1)));
            }
            current_statement_begin__ = 58;
            if (as_bool(logical_eq(phidist, 1))) {
                current_statement_begin__ = 58;
                lp_accum__.add(gamma_log<propto__>(phi, get_base1(prior_phi_param, 1, "prior_phi_param", 1), get_base1(prior_phi_param, 2, "prior_phi_param", 1)));
            }
            current_statement_begin__ = 59;
            if (as_bool(logical_eq(phidist, 2))) {
                current_statement_begin__ = 59;
                lp_accum__.add(cauchy_log<propto__>(phi, get_base1(prior_phi_param, 1, "prior_phi_param", 1), get_base1(prior_phi_param, 2, "prior_phi_param", 1)));
            }
            current_statement_begin__ = 60;
            if (as_bool(logical_gt(phidist, 2))) {
                current_statement_begin__ = 60;
                std::stringstream errmsg_stream__;
                errmsg_stream__ << "Wrong prior distribution for phi; found phidist=";
                errmsg_stream__ << phidist;
                throw std::domain_error(errmsg_stream__.str());
            }
            current_statement_begin__ = 63;
            for (int i = 1; i <= (sn - 1); ++i) {
                current_statement_begin__ = 64;
                for (int j = (i + 1); j <= sn; ++j) {
                    current_statement_begin__ = 65;
                    stan::model::assign(Sigma, 
                                stan::model::cons_list(stan::model::index_uni(i), stan::model::cons_list(stan::model::index_uni(j), stan::model::nil_index_list())), 
                                (sigma_sq * stan::math::exp(((-(1) * phi) * get_base1(dist, i, j, "dist", 1)))), 
                                "assigning variable Sigma");
                    current_statement_begin__ = 67;
                    stan::model::assign(Sigma, 
                                stan::model::cons_list(stan::model::index_uni(j), stan::model::cons_list(stan::model::index_uni(i), stan::model::nil_index_list())), 
                                get_base1(Sigma, i, j, "Sigma", 1), 
                                "assigning variable Sigma");
                }
            }
            current_statement_begin__ = 70;
            for (int i = 1; i <= sn; ++i) {
                current_statement_begin__ = 70;
                stan::model::assign(Sigma, 
                            stan::model::cons_list(stan::model::index_uni(i), stan::model::cons_list(stan::model::index_uni(i), stan::model::nil_index_list())), 
                            (sigma_sq + tau_sq), 
                            "assigning variable Sigma");
            }
            current_statement_begin__ = 71;
            stan::math::assign(L, cholesky_decompose(Sigma));
            current_statement_begin__ = 73;
            stan::math::assign(xbmodel, multiply(X, beta));
            current_statement_begin__ = 75;
            for (int i = 1; i <= ntobs; ++i) {
                current_statement_begin__ = 76;
                stan::model::assign(z1, 
                            stan::model::cons_list(stan::model::index_uni(get_base1(data_obs_idx, i, "data_obs_idx", 1)), stan::model::nil_index_list()), 
                            get_base1(yobs, i, "yobs", 1), 
                            "assigning variable z1");
            }
            current_statement_begin__ = 78;
            if (as_bool(logical_gt(missing, 0))) {
                current_statement_begin__ = 79;
                for (int k = 1; k <= ntmiss; ++k) {
                    current_statement_begin__ = 80;
                    stan::model::assign(z1, 
                                stan::model::cons_list(stan::model::index_uni(get_base1(data_miss_idx, k, "data_miss_idx", 1)), stan::model::nil_index_list()), 
                                get_base1(z_miss, k, "z_miss", 1), 
                                "assigning variable z1");
                }
            }
            current_statement_begin__ = 83;
            for (int i = 1; i <= tn; ++i) {
                current_statement_begin__ = 84;
                for (int j = 1; j <= sn; ++j) {
                    current_statement_begin__ = 85;
                    stan::model::assign(dats, 
                                stan::model::cons_list(stan::model::index_uni(i), stan::model::cons_list(stan::model::index_uni(j), stan::model::nil_index_list())), 
                                get_base1(z1, (i + ((j - 1) * tn)), "z1", 1), 
                                "assigning variable dats");
                    current_statement_begin__ = 86;
                    stan::model::assign(mus, 
                                stan::model::cons_list(stan::model::index_uni(i), stan::model::cons_list(stan::model::index_uni(j), stan::model::nil_index_list())), 
                                get_base1(xbmodel, (i + ((j - 1) * tn)), "xbmodel", 1), 
                                "assigning variable mus");
                }
            }
            current_statement_begin__ = 91;
            for (int t = 1; t <= tn; ++t) {
                current_statement_begin__ = 92;
                lp_accum__.add(multi_normal_cholesky_log<propto__>(get_base1(dats, t, "dats", 1), get_base1(mus, t, "mus", 1), L));
            }
            current_statement_begin__ = 96;
            if (as_bool(logical_gt(verbose, 0))) {
                current_statement_begin__ = 97;
                if (pstream__) {
                    stan_print(pstream__,"beta= ");
                    stan_print(pstream__,beta);
                    *pstream__ << std::endl;
                }
                current_statement_begin__ = 98;
                if (pstream__) {
                    stan_print(pstream__,"sigma sq = ");
                    stan_print(pstream__,sigma_sq);
                    *pstream__ << std::endl;
                }
                current_statement_begin__ = 99;
                if (pstream__) {
                    stan_print(pstream__,"tau sq = ");
                    stan_print(pstream__,tau_sq);
                    *pstream__ << std::endl;
                }
                current_statement_begin__ = 100;
                if (pstream__) {
                    stan_print(pstream__,"range = ");
                    stan_print(pstream__,(3.0 / phi));
                    *pstream__ << std::endl;
                }
            }
            }
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
        lp_accum__.add(lp__);
        return lp_accum__.sum();
    } // log_prob()
    template <bool propto, bool jacobian, typename T_>
    T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
               std::ostream* pstream = 0) const {
      std::vector<T_> vec_params_r;
      vec_params_r.reserve(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        vec_params_r.push_back(params_r(i));
      std::vector<int> vec_params_i;
      return log_prob<propto,jacobian,T_>(vec_params_r, vec_params_i, pstream);
    }
    void get_param_names(std::vector<std::string>& names__) const {
        names__.resize(0);
        names__.push_back("beta");
        names__.push_back("phi");
        names__.push_back("sigma_sq");
        names__.push_back("tau_sq");
        names__.push_back("z_miss");
    }
    void get_dims(std::vector<std::vector<size_t> >& dimss__) const {
        dimss__.resize(0);
        std::vector<size_t> dims__;
        dims__.resize(0);
        dims__.push_back(p);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back((missing ? ntmiss : 0 ));
        dimss__.push_back(dims__);
    }
    template <typename RNG>
    void write_array(RNG& base_rng__,
                     std::vector<double>& params_r__,
                     std::vector<int>& params_i__,
                     std::vector<double>& vars__,
                     bool include_tparams__ = true,
                     bool include_gqs__ = true,
                     std::ostream* pstream__ = 0) const {
        typedef double local_scalar_t__;
        vars__.resize(0);
        stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
        static const char* function__ = "model_gp_marginal_namespace::write_array";
        (void) function__;  // dummy to suppress unused var warning
        // read-transform, write parameters
        Eigen::Matrix<double, Eigen::Dynamic, 1> beta = in__.vector_constrain(p);
        size_t beta_j_1_max__ = p;
        for (size_t j_1__ = 0; j_1__ < beta_j_1_max__; ++j_1__) {
            vars__.push_back(beta(j_1__));
        }
        double phi = in__.scalar_lb_constrain(0);
        vars__.push_back(phi);
        double sigma_sq = in__.scalar_lb_constrain(0);
        vars__.push_back(sigma_sq);
        double tau_sq = in__.scalar_lb_constrain(0);
        vars__.push_back(tau_sq);
        std::vector<double> z_miss;
        size_t z_miss_d_0_max__ = (missing ? ntmiss : 0 );
        z_miss.reserve(z_miss_d_0_max__);
        for (size_t d_0__ = 0; d_0__ < z_miss_d_0_max__; ++d_0__) {
            z_miss.push_back(in__.scalar_constrain());
        }
        size_t z_miss_k_0_max__ = (missing ? ntmiss : 0 );
        for (size_t k_0__ = 0; k_0__ < z_miss_k_0_max__; ++k_0__) {
            vars__.push_back(z_miss[k_0__]);
        }
        double lp__ = 0.0;
        (void) lp__;  // dummy to suppress unused var warning
        stan::math::accumulator<double> lp_accum__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        if (!include_tparams__ && !include_gqs__) return;
        try {
            if (!include_gqs__ && !include_tparams__) return;
            if (!include_gqs__) return;
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }
    template <typename RNG>
    void write_array(RNG& base_rng,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                     bool include_tparams = true,
                     bool include_gqs = true,
                     std::ostream* pstream = 0) const {
      std::vector<double> params_r_vec(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r_vec[i] = params_r(i);
      std::vector<double> vars_vec;
      std::vector<int> params_i_vec;
      write_array(base_rng, params_r_vec, params_i_vec, vars_vec, include_tparams, include_gqs, pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i)
        vars(i) = vars_vec[i];
    }
    std::string model_name() const {
        return "model_gp_marginal";
    }
    void constrained_param_names(std::vector<std::string>& param_names__,
                                 bool include_tparams__ = true,
                                 bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        size_t beta_j_1_max__ = p;
        for (size_t j_1__ = 0; j_1__ < beta_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "beta" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        param_name_stream__.str(std::string());
        param_name_stream__ << "phi";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "sigma_sq";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "tau_sq";
        param_names__.push_back(param_name_stream__.str());
        size_t z_miss_k_0_max__ = (missing ? ntmiss : 0 );
        for (size_t k_0__ = 0; k_0__ < z_miss_k_0_max__; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "z_miss" << '.' << k_0__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        if (!include_gqs__ && !include_tparams__) return;
        if (include_tparams__) {
        }
        if (!include_gqs__) return;
    }
    void unconstrained_param_names(std::vector<std::string>& param_names__,
                                   bool include_tparams__ = true,
                                   bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        size_t beta_j_1_max__ = p;
        for (size_t j_1__ = 0; j_1__ < beta_j_1_max__; ++j_1__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "beta" << '.' << j_1__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        param_name_stream__.str(std::string());
        param_name_stream__ << "phi";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "sigma_sq";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "tau_sq";
        param_names__.push_back(param_name_stream__.str());
        size_t z_miss_k_0_max__ = (missing ? ntmiss : 0 );
        for (size_t k_0__ = 0; k_0__ < z_miss_k_0_max__; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "z_miss" << '.' << k_0__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        if (!include_gqs__ && !include_tparams__) return;
        if (include_tparams__) {
        }
        if (!include_gqs__) return;
    }
}; // model
}  // namespace
typedef model_gp_marginal_namespace::model_gp_marginal stan_model;
#ifndef USING_R
stan::model::model_base& new_model(
        stan::io::var_context& data_context,
        unsigned int seed,
        std::ostream* msg_stream) {
  stan_model* m = new stan_model(data_context, seed, msg_stream);
  return *m;
}
#endif
#endif
