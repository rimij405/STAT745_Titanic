# STAT745: Titanic Survival Classification

Group term project regarding the [Titanic dataset from Kaggle](https://www.kaggle.com/c/titanic/overview).

## Getting Started

Due to licensing restrictions on hosting data from Kaggle competitions, the data is not included in this repository.

### (Option 1) Download Data Manually

You can download the competition data from the [Data](https://www.kaggle.com/c/titanic/data) tab on the competition page.

### (Option 2) Use the Kaggle API

If you have the [Kaggle API](#) and [Python 3](#) installed, you can have the competition data automatically downloaded to the appropriate `data-raw/` folder location.

After you install the [Kaggle API](#), you should add your authentication details to your device. Either add the `kaggle.json` file to a `~/.kaggle/` folder in your home directory or simply export your credentials to environment variables:

```bash
export KAGGLE_USERNAME=<USERNAME>
export KAGGLE_KEY=<API TOKEN>
```

Once you can authenticate, simply call the `kaggle` command below to get access to the data:

```bash
$ kaggle c download --path ./data-raw/ titanic
```

Now, run the `titanic.R` script inside of the `data-raw/` directory to get the contents into your `data/` folder, ready for use in the project.


