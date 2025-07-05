Efficiency_beta = function(alpha)
{
  z = (1+(alpha^2)/(1+2*alpha))^(-3/2)*100
  return(z)
}

Efficiency_sigma = function(alpha)
{
  z = (2+alpha^2)^2/2
  y = 2*(1+2*alpha^2)*(1+(alpha^2)/(1+alpha*2))^(5/2)
  x = alpha^2*(1+alpha)^2
  h=z*(y-x)^(-1)*100
  return(h)
}

Efficiency_beta(1)
Efficiency_sigma(1)
