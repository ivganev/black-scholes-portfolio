/////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////
/// 
/// 
///     



    // Plot price versus time to expiration
    // Plot delta versus time to expiration
    // Plot gamma versus time to expiration
    // Plot theta versus time to expiration
    // Plot vega versus time to expiration
    // Plot rho versus time to expiration


fn plot_over_time(&self, number_samples: i32, greek: Greek) -> Result<(), Box<dyn std::error::Error>> {

    let T = self.time_to_expiry;
    let xdata: Vec<f32> = (0..number_samples).map( |i| (i as f32)*T/(number_samples as f32)).collect();
    let mut ydata = vec![0f32; number_samples as usize];

    let r = self.interest_rate;
    let sigma = self.volatility;
    let n = Normal::new(0.0, 1.0).unwrap(); // Make this into a global variable somehow?

    for op in &self.options {
        let t = op.time_to_expiry;
        let k = op.strike_price;
        let q = op.quantity;

        // Neet to worry about if the option has already expired.

    }

    return plot(
        (0..100).map(|x| x as f32).collect(), 
        vec![5f32; 100], 
        "Fun plot".to_string(), 
        "plotters-doc-data/5.png".to_string(),
        50.0,
        "fun independent variable".to_string(),
        "fun dependent variable".to_string()
    );

}

///// SCRAPS
/// //////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////
/// 
/// 

fn call_price(t: f32, x: f32, k: f32, sigma: f32, r: f32) -> f64 {
    let dplus = ((x/k).ln() +  (r + sigma*sigma*0.5)*t )/(sigma*(t.sqrt()));
    let dminus = ((x/k).ln() +  (r - sigma*sigma*0.5)*t )/(sigma*(t.sqrt()));

    let t = t as f64;
    let x = x as f64;
    let k = k as f64;
    let sigma = sigma as f64;
    let r = r as f64;

    let n = Normal::new(0.0, 1.0).unwrap();
    return x*n.cdf(dplus as f64) - ((-r*t).exp())*k*n.cdf(dminus as f64);
}




fn call_greeks(t: f32, x: f32, k: f32, sigma: f32, r: f32) -> HashMap<String, f64> {
    let dplus = ((x/k).ln() +  (r + sigma*sigma*0.5)*t )/(sigma*(t.sqrt()));
    let dminus = ((x/k).ln() +  (r - sigma*sigma*0.5)*t )/(sigma*(t.sqrt()));

    let t = t as f64;
    let x = x as f64;
    let k = k as f64;
    let sigma = sigma as f64;
    let r = r as f64;

    let n = Normal::new(0.0, 1.0).unwrap();
    let price = x*n.cdf(dplus as f64) - ((-r*t).exp())*k*n.cdf(dminus as f64);
    let delta = n.cdf(dplus as f64);
    let gamma = n.pdf(dplus as f64)/(x*sigma*t.sqrt());
    let theta = -(x*sigma*n.pdf(dplus as f64))/(2.0*t.sqrt()) - r*k*((-r*t).exp())*n.cdf(dminus as f64);
    let vega = x*n.pdf(dplus as f64)*t.sqrt();
    let rho = t*k*((-r*t).exp())*n.cdf(dminus as f64);

    let mut greeks: HashMap<String, f64> = HashMap::new();
    greeks.insert("price".to_string(), price);
    greeks.insert("delta".to_string(), delta);
    greeks.insert("gamma".to_string(), gamma);
    greeks.insert("theta".to_string(), theta);
    greeks.insert("vega".to_string(), vega);
    greeks.insert("rho".to_string(), rho);
    return greeks;
}

fn put_greeks(t: f32, x: f32, k: f32, sigma: f32, r: f32) -> HashMap<String, f64> {
    let dplus = ((x/k).ln() +  (r + sigma*sigma*0.5)*t )/(sigma*(t.sqrt()));
    let dminus = ((x/k).ln() +  (r - sigma*sigma*0.5)*t )/(sigma*(t.sqrt()));

    let t = t as f64;
    let x = x as f64;
    let k = k as f64;
    let sigma = sigma as f64;
    let r = r as f64;

    let n = Normal::new(0.0, 1.0).unwrap();
    let price = -x*n.cdf(-dplus as f64) + ((-r*t).exp())*k*n.cdf(-dminus as f64);
    let delta = n.cdf((dplus - 1.0 ) as f64);
    let gamma = n.pdf(dplus as f64)/(x*sigma*t.sqrt());
    let theta = -(x*sigma*n.pdf(dplus as f64))/(2.0*t.sqrt()) + r*k*((-r*t).exp())*n.cdf(-dminus as f64);
    let vega = x*n.pdf(dplus as f64)*t.sqrt();
    let rho = -t*k*((-r*t).exp())*n.cdf(-dminus as f64);

    let mut greeks: HashMap<String, f64> = HashMap::new();
    greeks.insert("price".to_string(), price);
    greeks.insert("delta".to_string(), delta);
    greeks.insert("gamma".to_string(), gamma);
    greeks.insert("theta".to_string(), theta);
    greeks.insert("vega".to_string(), vega);
    greeks.insert("rho".to_string(), rho);
    return greeks;
}

struct InterestRate {
    rate: f32,
    // This is the annual interest rate. 
}

impl InterestRate {
    fn summary(&self) -> String {
        return format!("The annual interest rate is {}.", self.rate);
    }
}

impl Opt {
    fn summary(&self) -> String {
        match self.kind {
            OptKind::Call => {
                return format!("This is a call option with stike {} expiring in {} year(s).", self.strike_price, self.time_to_expiry)
            },
            OptKind::Put => {
                return format!("This is a put option with stike {} expiring in {} year(s).", self.strike_price, self.time_to_expiry)
            },
        }
    }
}


fn fun_plot() {
    plot(
        (0..100).map(|x| x as f32).collect(), 
        vec![5f32; 100], 
        "Fun plot".to_string(), 
        "plotters-doc-data/5.png".to_string(),
        50.0,
        "fun independent variable".to_string(),
        "fun dependent variable".to_string()
    );
}