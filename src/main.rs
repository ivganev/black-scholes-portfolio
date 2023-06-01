use statrs::distribution::Normal;
use statrs::distribution::Continuous;
use statrs::distribution::ContinuousCDF;
use std::collections::HashMap;
use plotters::prelude::*;

#[allow(dead_code)]

////////////////////////////////////
/// Structs 
////////////////////////////////////

struct Portfolio {
    /* A portfolio indicates: 
    the spot price and volatility of the underlying asset, 
    the annual interest rate, which is assumed to be constant, 
    the number of contracts for the underlying asset,
    the options that are part of the portfolio,
    the cash position
     */
    name: String,
    spot: f32,
    volatility: f32,
    interest_rate: f32,
    contracts: f32,
    options: Vec<Opt>,
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

enum Greek{
    Price,
    Delta,
    Gamma, 
    Theta,
    Vega,
    Rho
}

fn greek_string(greek: &Greek) -> String {
    match greek {
        Greek::Price => String::from("Value"),
        Greek::Delta => String::from("Delta"),
        Greek::Gamma => String::from("Gamma"),
        Greek::Theta => String::from("Theta"),
        Greek::Vega => String::from("Vega"),
        Greek::Rho => String::from("Rho"),
    }
}

////////////////////////////////////
/// Portfolio methods 
////////////////////////////////////

impl Portfolio {

    /// Summarize the properties of the underlying
    fn underlying_summary(&self) -> String {
        return format!("The underlying has current price {} with volatiilty {}.", self.spot, self.volatility);
    }

    /// Add an option to the portfolio
    fn add_option(&mut self, op: Opt) {
        if op.time_to_expiry > self.time_to_expiry {
            self.time_to_expiry = op.time_to_expiry;
        }
        self.options.push(op);
    }

    /// Summary of portfolio
    fn summary(&self) {
        println!("Here is a summary of the portfolio \"{}\":",  self.name);
        println!("  {} contracts currently worth {}", self.contracts, self.spot);
        //println!("  {} in cash", self.cash);
        for op in &self.options {
            let mut kind = "call".to_string();
            match op.kind {
                OptKind::Put => kind = "put".to_string(),
                OptKind::Call => kind = "call".to_string()
            }
            println!("  {} {} options with stike {} expiring in {} year(s)", 
            op.quantity, kind, op.strike_price, op.time_to_expiry);
        }
    }

    /// Compute greeks
    fn greeks(&self) -> HashMap<String, f32> {
        let x = self.spot;
        let r = self.interest_rate;
        let sigma = self.volatility;

        let mut price = self.contracts*self.spot;
        let mut delta = self.contracts;
        let mut gamma = 0.0;
        let mut theta = 0.0;
        let mut vega = 0.0;
        let mut rho = 0.0;
        let n = Normal::new(0.0, 1.0).unwrap();

        for op in &self.options {
            let t =  op.time_to_expiry;
            let k = op.strike_price;
            let q = op.quantity;

            let dplus = ((x/k).ln() +  (r + sigma*sigma*0.5)*t )/(sigma*(t.sqrt()));
            let dminus = ((x/k).ln() +  (r - sigma*sigma*0.5)*t )/(sigma*(t.sqrt()));

            gamma += q*(n.pdf(dplus as f64) as f32)/(x*sigma*t.sqrt());
            vega += q*x*(n.pdf(dplus as f64) as f32)*t.sqrt();

            match op.kind {
                OptKind::Call => {
                    price += q*(x*(n.cdf(dplus as f64) as f32) - ((-r*t).exp())*k*(n.cdf(dminus as f64) as f32));
                    delta += q*(n.cdf(dplus as f64) as f32);
                    theta += q*(-(x*sigma*(n.pdf(dplus as f64) as f32))/(2.0*t.sqrt()) - r*k*((-r*t).exp())*(n.cdf(dminus as f64) as f32));
                    rho += q*t*k*((-r*t).exp())*(n.cdf(dminus as f64) as f32);
                },
                OptKind::Put => {
                    price += q*(-x*(n.cdf(-dplus as f64) as f32) + ((-r*t).exp())*k*(n.cdf(-dminus as f64) as f32));
                    delta += q*(n.cdf((dplus - 1.0 ) as f64)as f32);
                    theta += q*(-(x*sigma*(n.pdf(dplus as f64) as f32))/(2.0*t.sqrt()) + r*k*((-r*t).exp())*(n.cdf(-dminus as f64) as f32));
                    rho += -q*t*k*((-r*t).exp())*(n.cdf(-dminus as f64) as f32);                
                },
            }
        }

        let mut greeks: HashMap<String, f32> = HashMap::new();
        greeks.insert("price".to_string(), price);
        greeks.insert("delta".to_string(), delta);
        greeks.insert("gamma".to_string(), gamma);
        greeks.insert("theta".to_string(), theta);
        greeks.insert("theta_by_day".to_string(), theta/365.0);
        greeks.insert("vega".to_string(), vega);
        greeks.insert("rho".to_string(), rho);
        return greeks;
    }

    /// Find the three standard deviations range from the spot price
    fn likely_underlying_range(&self) -> [f32; 2] {
        let mut lower = 0.0;
        if self.volatility < 1.0/3.0 {
            lower = self.spot*(1.0 - 3.0*self.volatility);
        }
        return [lower, self.spot*(1.0  + 3.0*self.volatility)];
    }
    
    /// Plot changes in greeks as the underlying changes
    fn plot_over_underlying(&self, number_samples: i32, greek: Greek) -> Result<(), Box<dyn std::error::Error>> {
        let [lower, upper] = self.likely_underlying_range();
        let increment_size = (upper - lower)/(number_samples as f32);

        let xdata: Vec<f32> = (0..number_samples).map( |i| (lower + (i as f32)*increment_size)).collect();
        let mut ydata = vec![0f32; number_samples as usize];

        match greek {
            Greek::Price => {        
                for i in 0..(number_samples) {
                    ydata[i as usize] += self.contracts*(lower + (i as f32)*increment_size);
                }
            }, 
            Greek::Delta => {        
                for i in 0..(number_samples) {
                    ydata[i as usize] += self.contracts;
                }
            }, 
            _ => ()
        }


        let r = self.interest_rate;
        let sigma = self.volatility;
        let n = Normal::new(0.0, 1.0).unwrap(); // Make this into a global variable somehow?

        for op in &self.options {
            let t = op.time_to_expiry;
            let k = op.strike_price;
            let q = op.quantity;

            for i in 0..(number_samples) {
                let x = lower + (i as f32)*increment_size;
                let dplus = ((x/k).ln() +  (r + sigma*sigma*0.5)*t )/(sigma*(t.sqrt()));
                let dminus = ((x/k).ln() +  (r - sigma*sigma*0.5)*t )/(sigma*(t.sqrt()));
                match op.kind {
                    OptKind::Call => {
                        ydata[i as usize] += q*compute_greek_call(x, k, t, r, sigma, dplus, dminus , n, &greek);
                    },
                    OptKind::Put => {
                        ydata[i as usize] += q*compute_greek_put(x, k, t, r, sigma, dplus, dminus, n, &greek);
                    },
                }
            }
        }

        return plot(
            xdata,
            ydata,
            format!("Change in Portfolio {}", greek_string(&greek)), 
            format!("plotters-doc-data/{}.png", greek_string(&greek).to_lowercase()),
            self.spot,
            format!("Price of underlying (current = {})", self.spot),
            format!("Portfolio {}" , greek_string(&greek))
        ); 
    }
    

}

fn compute_greek_call(x: f32, k: f32, t: f32, r: f32, sigma: f32, dp: f32, dm: f32, n: Normal, gr: &Greek) -> f32 {
    match gr {
        Greek::Price => return x*(n.cdf(dp as f64) as f32) - ((-r*t).exp())*k*(n.cdf(dm as f64) as f32),
        Greek::Delta => return n.cdf(dp as f64) as f32,
        Greek::Gamma => return (n.pdf(dp as f64) as f32)/(x*sigma*t.sqrt()),
        Greek::Theta => return -(x*sigma*(n.pdf(dp as f64) as f32))/(2.0*t.sqrt()) - r*k*((-r*t).exp())*(n.cdf(dm as f64) as f32),
        Greek::Vega => return x*(n.pdf(dp as f64) as f32)*t.sqrt(),
        Greek::Rho => return t*k*((-r*t).exp())*(n.cdf(dm as f64) as f32),
    }
}

fn compute_greek_put(x: f32, k: f32, t: f32, r: f32, sigma: f32, dp: f32, dm: f32, n: Normal, gr: &Greek) -> f32 {
    match gr {
        Greek::Price => return -x*(n.cdf(-dp as f64) as f32) + ((-r*t).exp())*k*(n.cdf(-dm as f64) as f32),
        Greek::Delta => return n.cdf((dp - 1.0 ) as f64) as f32,
        Greek::Gamma => return (n.pdf(dp as f64) as f32)/(x*sigma*t.sqrt()),
        Greek::Theta => return -(x*sigma*(n.pdf(dp as f64) as f32))/(2.0*t.sqrt()) + r*k*((-r*t).exp())*(n.cdf(-dm as f64) as f32),
        Greek::Vega => return x*(n.pdf(dp as f64) as f32)*t.sqrt(),
        Greek::Rho => return -t*k*((-r*t).exp())*(n.cdf(-dm as f64) as f32),
    }
}


///////////////////////////////////////
/// Plot function
///////////////////////////////////////

fn plot(xdata: Vec<f32>, ydata: Vec<f32>, title: String, file_path: String, current_price: f32, xlabel: String, ylabel: String) -> Result<(), Box<dyn std::error::Error>> {

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
    let range = yupper - ylower;
    yupper += range*0.2;
    ylower -= range*0.2;

    let root = BitMapBackend::new(&file_path, (640, 480)).into_drawing_area();
    root.fill(&WHITE)?;
    let mut chart = ChartBuilder::on(&root)
        .caption(title, ("sans-serif", 30).into_font())
        .margin(10)
        .x_label_area_size(50)
        .y_label_area_size(60)
        .build_cartesian_2d(xdata[0]..xdata[number_samples-1], (ylower)..(yupper))?;

    chart.configure_mesh()
        .y_labels(10)
        .x_labels(10)
        .light_line_style(&TRANSPARENT)
        .x_desc(xlabel)
        .y_desc(ylabel)
        .draw()?;
    
    chart
        .draw_series(LineSeries::new((0..(number_samples)).map(|i| (xdata[i], ydata[i])), &BLUE))?;

    chart.draw_series(LineSeries::new(
        vec![(current_price, ylower), (current_price, yupper)],
        &BLACK,
    ))?;

    root.present()?;
    Ok(())
}

///////////////////////////////////////
/// MAIN 
//////////////////////////////////// 

fn main() {

    let mut portfolio1 = Portfolio {
        name: "My portfolio".to_string(),
        spot: 97.50,
        volatility: 0.25,
        interest_rate: 0.06, 
        contracts:  7.0,
        options: Vec::new(),
        time_to_expiry: 0.0,
    };
    
    portfolio1.add_option(Opt { 
        kind: OptKind::Call, strike_price: 95.0, time_to_expiry: 91.0/365.0, quantity: 10.0 
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
    portfolio1.plot_over_underlying(30, Greek::Price);
    portfolio1.plot_over_underlying(30, Greek::Delta);
    portfolio1.plot_over_underlying(30, Greek::Gamma);
    portfolio1.plot_over_underlying(30, Greek::Theta);
    portfolio1.plot_over_underlying(30, Greek::Vega);
    portfolio1.plot_over_underlying(30, Greek::Rho);


}


