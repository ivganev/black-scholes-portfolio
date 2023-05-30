use statrs::distribution::Normal;
use statrs::distribution::Continuous;
use statrs::distribution::ContinuousCDF;
use std::collections::HashMap;
use plotters::prelude::*;

#[allow(dead_code)]

struct Portfolio {
    /* A portfolio indicates: 
    the spot price and volatility of the underlying asset, 
    the annual interest rate, which is assumed to be constant, 
    the number of contracts for the underlying asset,
    the options that are part of the portfolio,
    the cash position
     */
    spot: f32,
    volatility: f32,
    interest_rate: f32,
    contracts: f32,
    options: Vec<Opt>,
    cash: f32,
    time_to_expiry: f32,
}

enum OptKind {
    Call,
    Put,
}

struct Opt {
    /* An option has a type (put or call), a strike price, 
    an expiration date, and a quantity (which can be negative). */
    kind: OptKind,
    strike_price: f32,
    time_to_expiry: f32,
    quantity: f32,
}

impl Portfolio {
    fn underlying_summary(&self) -> String {
        return format!("The underlying has current price {} with volatiilty {}.", self.spot, self.volatility);
    }

    fn likely_underlying_range(&self) -> [f32; 2] {
        let mut lower = 0.0;
        if self.volatility < 1.0/3.0 {
            lower = self.spot*(1.0 - 3.0*self.volatility);
        }
        return [lower, self.spot*(1.0  + 3.0*self.volatility)];
    }

    fn add_option(&mut self, op: Opt) {
        if op.time_to_expiry > self.time_to_expiry {
            self.time_to_expiry = op.time_to_expiry;
        }
        self.options.push(op);
    }

    fn plot_price_changes(&self, number_samples: i32) -> Result<(), Box<dyn std::error::Error>> {
        let [lower, upper] = self.likely_underlying_range();
        let increment_size = (upper - lower)/(number_samples as f32);

        let xdata: Vec<f32> = (0..number_samples).map( |i| (lower + (i as f32)*increment_size)).collect();
        let mut ydata = vec![0f32; number_samples as usize];

        let r = self.interest_rate;
        let sigma = self.volatility;
        let n = Normal::new(0.0, 1.0).unwrap();

        for op in &self.options {
            let t =  op.time_to_expiry;
            let k = op.strike_price;
            let q = op.quantity;

            for i in 0..(number_samples) {
                let x = lower + (i as f32)*increment_size;
                let dplus = ((x/k).ln() +  (r + sigma*sigma*0.5)*t )/(sigma*(t.sqrt()));
                let dminus = ((x/k).ln() +  (r - sigma*sigma*0.5)*t )/(sigma*(t.sqrt()));
                match op.kind {
                    OptKind::Call => {
                        ydata[i as usize] += q*(x*(n.cdf(dplus as f64) as f32) - ((-r*t).exp())*k*(n.cdf(dminus as f64) as f32));
                    },
                    OptKind::Put => {
                        ydata[i as usize] += q*(-x*(n.cdf(-dplus as f64) as f32) + ((-r*t).exp())*k*(n.cdf(-dminus as f64) as f32));            
                    },
                }
            }
        }

        return plot(
            xdata,
            ydata,
            "Portfolio price as underlying varies".to_string(),
            "plotters-doc-data/price.png".to_string()
        ); 
    }

    fn summary(&self) {
        println!("Here is a summary of the portfolio:");
        println!("  {} contracts currently worth {}", self.contracts, self.spot);
        println!("  {} in cash", self.cash);
        for op in &self.options {
            let mut kind = "call".to_string();
            match op.kind {
                OptKind::Put => kind = "put".to_string(),
                _ => {}
            }
            println!("  {} {} options with stike {} expiring in {} year(s)", 
            op.quantity, kind, op.strike_price, op.time_to_expiry);
        }
    }

    fn greeks(&self) -> HashMap<String, f64> {
        let x = self.spot as f64;
        let r = self.interest_rate as f64;
        let sigma = self.volatility as f64;

        let price = self.contracts*self.spot;
        let mut price = price as f64;
        let mut delta = self.contracts as f64;
        let mut gamma = 0.0;
        let mut theta = 0.0;
        let mut vega = 0.0;
        let mut rho = 0.0;
        let n = Normal::new(0.0, 1.0).unwrap();

        for op in &self.options {
            let t =  op.time_to_expiry as f64;
            let k = op.strike_price as f64;
            let q = op.quantity as f64;

            let dplus = ((x/k).ln() +  (r + sigma*sigma*0.5)*t )/(sigma*(t.sqrt()));
            let dminus = ((x/k).ln() +  (r - sigma*sigma*0.5)*t )/(sigma*(t.sqrt()));

            gamma += q*n.pdf(dplus as f64)/(x*sigma*t.sqrt());
            vega += q*x*n.pdf(dplus as f64)*t.sqrt();

            match op.kind {
                OptKind::Call => {
                    price += q*(x*n.cdf(dplus as f64) - ((-r*t).exp())*k*n.cdf(dminus as f64));
                    delta += q*n.cdf(dplus as f64);
                    theta += q*(-(x*sigma*n.pdf(dplus as f64))/(2.0*t.sqrt()) - r*k*((-r*t).exp())*n.cdf(dminus as f64));
                    rho += q*t*k*((-r*t).exp())*n.cdf(dminus as f64);
                },
                OptKind::Put => {
                    price += q*(-x*n.cdf(-dplus as f64) + ((-r*t).exp())*k*n.cdf(-dminus as f64));
                    delta += q*n.cdf((dplus - 1.0 ) as f64);
                    theta += q*(-(x*sigma*n.pdf(dplus as f64))/(2.0*t.sqrt()) + r*k*((-r*t).exp())*n.cdf(-dminus as f64));
                    rho += -q*t*k*((-r*t).exp())*n.cdf(-dminus as f64);                
                },
            }
        }

        /* Theta is sometimes given in units of days, so divide by 365. */

        let mut greeks: HashMap<String, f64> = HashMap::new();
        greeks.insert("price".to_string(), price);
        greeks.insert("delta".to_string(), delta);
        greeks.insert("gamma".to_string(), gamma);
        greeks.insert("theta".to_string(), theta);
        greeks.insert("theta_by_day".to_string(), theta/365.0);
        greeks.insert("vega".to_string(), vega);
        greeks.insert("rho".to_string(), rho);
        return greeks;

    }
}

///////////////////////////////////////

fn plot(xdata: Vec<f32>, ydata: Vec<f32>, title: String, file_path: String) -> Result<(), Box<dyn std::error::Error>> {

    assert_eq!(xdata.len(), ydata.len());
    let number_samples = xdata.len();

    let mut ylower = ydata[0];
    let mut yupper = ydata[0];
    for i in 1..(number_samples) {
        if ydata[i as usize] < ylower {
            ylower = ydata[i as usize];
        }
        if ydata[i as usize] > yupper {
            yupper = ydata[i as usize];
        }
    }
    yupper = yupper + (yupper - ylower)*0.2;
    ylower = ylower - (yupper - ylower)*0.2;

    let root = BitMapBackend::new(&file_path, (640, 480)).into_drawing_area();
    root.fill(&WHITE)?;
    let mut chart = ChartBuilder::on(&root)
        .caption(title, ("sans-serif", 30).into_font())
        .margin(5)
        .x_label_area_size(30)
        .y_label_area_size(30)
        .build_cartesian_2d(xdata[0]..xdata[number_samples-1], (ylower)..(yupper))?;

    chart.configure_mesh()
        .y_labels(10)
        .light_line_style(&TRANSPARENT)
        .disable_x_mesh()
        .draw()?;
    
    chart
        .draw_series(LineSeries::new((0..(number_samples)).map(|i| (xdata[i], ydata[i])), &RED))?;

    root.present()?;
    Ok(())
}

///////////////////////////////////////
/// MAIN 
//////////////////////////////////// 

fn main() {

    let mut portfolio1 = Portfolio {
        spot: 99.50,
        volatility: 0.25,
        interest_rate: 0.06, 
        contracts:  7.0,
        options: Vec::new(),
        cash: 0.0,
        time_to_expiry: 0.0,
    };
    
    portfolio1.add_option(Opt { 
        kind: OptKind::Call, strike_price: 95.0, time_to_expiry: 91.0/365.0, quantity: -10.0 
    });
    portfolio1.add_option(Opt { 
        kind: OptKind::Call, strike_price: 100.0, time_to_expiry: 1.0, quantity: 3.0 
    });
    portfolio1.add_option(Opt { 
        kind: OptKind::Put, strike_price: 90.0, time_to_expiry: 1.0, quantity: -2.5 
    });
    portfolio1.add_option(Opt { 
        kind: OptKind::Put, strike_price: 85.0, time_to_expiry: 0.75, quantity: 2.0 
    });

    println!("{}", portfolio1.underlying_summary());
    println!("{:?}", portfolio1.greeks());
    portfolio1.summary();
    portfolio1.plot_price_changes(30);

    plot(
        (0..100).map(|x| x as f32).collect(), 
        vec![5f32; 100], 
        "Fun plot".to_string(), 
        "plotters-doc-data/5.png".to_string()
    );
}


///////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////
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
