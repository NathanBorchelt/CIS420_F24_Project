# AFRL Cluster Design

This project is an updated version of the Cluster Design software found at https://clusterdesign.org/ with more data and increased modularity

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

- Windows or Linux
- Python 3.8+

### Installing

1. Clone the repository to your local machine
2. Open clusterdesign/bin/clusterdesign to run the program

### Usage

The Cluster Design program has 7 different tabs: Nodes, Performance, Network, UPS, Design, Settings, and About

#### Nodes

Impose constraints on your system like minimum or maximum cores

#### Performance

Direct a performance model based on cores

#### Network

Configure the network design model

#### UPS

Calculate the UPS cost using the power of the designed system

#### Design

Set global constraints like max power. Additionally, set the objective function to either TCO or CapEx

#### Settings
Set rack height, system lifetime, kW/hr price, and operating expenses

#### About

Information about Cluster Design

## Deployment

To update the software, push any changes back to the main branch of the git repository

## Documentation

The documentation for this software was generated using Doxygen documentation software and can be found [here](file:///html/index.html)

## Built With

* [Tkinter](https://pythonbasics.org/tkinter/) - The GUI framework used
* [Scrapy](https://scrapy.org/) - Web scraping software
* [Doxygen](https://www.doxygen.nl/index.html) Documentation generation

## Authors

* **Konstantin S. Solnushkin** - *Initial work* - [ClusterDesign](https://clusterdesign.org/)

## License

This project is licensed under the [GNU GPL License](https://www.gnu.org/licenses/gpl-3.0.en.html#license-text)
