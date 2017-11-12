
# @fama1973risk

*Risk, Return, and Equilibrium: Empirical Tests* 

Investors look at individual assets only in terms of covariance with the current portfolio.
Under the assumption that return distributions are normal, the risk of an asset is measured by its standard deviation.
$$
E(r_p) = \sum_{i=1}^N x_iE(r_i)
$$
$$
\sigma(r_p) = \sum_{i=1}^N x_i
\left[ \frac{\sum_{j=1}^N x_j \sigma_{ij}}{\sigma(r_p)} \right] =
\sum_{i=1}^N x_i \frac{Cov(r_i, r_p)}{\sigma(r_p)}
$$
The risk of an asset is proportional to its covariance with the current portfolio over the standard deviation of the current portfolio^[$Var(r_p)=\sigma^2(r_p)=\sum\sum w_iw_j\sigma_{ij}$ therefore $\sigma(r_p)=\sum w_i\frac{\sum w_j\sigma_{ij}}{\sigma(r_p)}$.
].
The risk of an asset is different for each portfolio.
Lagrangian methods say weights of each asset must satisfy
$$
E(r_i) - E(r_p) = S_p
\left[
\frac{\sum_{j=1}^Nx_j\sigma_{ij}}{\sigma(r_p)}-\sigma(r_p)
\right]
$$ {#eq:1}
where $S_p$ is the change in $E(r_p)$ for a change in $\sigma(r_p)$.
It is the slope of the efficient frontier at the point corresponding to portfolio $p$.
$S_p$ is the increase in expected return on the portfolio for an increase in the risk.
The difference between the return on the asset and the return on the portfolio is proportional to the difference between the risk of the asset and the risk of the portfolio, where the risk of the asset is measured as the additional risk to the portfolio.
$$
E(r_i) = E(r_p) - S_p\sigma(r_p) + \beta_iS_p\sigma(r_p)
$$ {#eq:2}
where
$$
\beta_i = \frac{Cov(r_i, r_p)}{\sigma^2(r_p)} =
    \frac{\sum_{j=1}^Nx_j\sigma_{ij}}{\sigma^2(r_p)} =
    \frac{\frac{\sum_{j=1}^Nx_j\sigma_{ij}}{\sigma(r_p)}}{\sigma(r_p)}
$$
$\beta_i$ refers to the risk of asset $i$ in portfolio $p$ relative to the total risk of $p$.
The intercept in +@eq:2 can be written as
$$
E(r_0) = E(r_p) - S_p\sigma(r_p)
$$ {#eq:3}
This is the return on a security whose return is uncorrelated with $r_p$ (a zero-$\beta$ security).
Since a zero-$\beta$ security does not change the risk of $p$, we can say it is riskless.
This does not mean the security has zero variance of return.
Based on +@eq:3 we can say
$$
S_p = \frac{E(r_p) - E(r_0)}{\sigma(r_p)}
$$ {#eq:sr}
and
$$
E(r_i) = E(r_0) + \left[E(r_p) - E(r_0)\right]\beta_i
$$ {#eq:mm}

*@eq:sr is the Sharpe Ratio of the portfolio $p$.
*@eq:mm has three testable implications;

- 
