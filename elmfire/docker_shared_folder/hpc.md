# Imperial College HPC Quick Reference

## Terms
**HPC**: High-Performance Computing cluster (many powerful computers, called nodes).
- **Login node**: Where you connect (the "front desk").
- **Compute node**: Where jobs actually run.
- **PBS**: Job scheduler (Portable Batch System).
- **$HOME**: Your personal storage, home directory.
- **EPHEMERAL/$TMPDIR**: Fast, temporary storage (wiped after job ends).
- **/rds/**: Research Data Store (for large, persistent files).
- **Local**: your Mac.  
- **Remote**: the HPC.
---

## Connecting to the HPC
### Terminal
```bash
ssh mb3324@login-b.cx3.hpc.ic.ac.uk
```
Could also use VSCode to connect.
---

## Transferring Files
To hpc/remote:
% rsync -rvltoD /Users/mb3324/Desktop/irp-mb3324/scratch/hface-cyl-scaled/jupyter-pckg mb3324@dtn-b.hpc.ic.ac.uk:/rds/general/user/mb3324/home/
To local:
% rsync -rvltoD mb3324@dtn-b.hpc.ic.ac.uk:/rds/general/user/mb3324/home/jupyter-pckg /Users/mb3324/Desktop/irp-mb3324/scratch/hface-cyl-scaled/

rsync -rvltoD mb3324@dtn-b.hpc.ic.ac.uk:/rds/general/user/mb3324/home/cyl-scaled-bc /Users/mb3324/Desktop/irp-mb3324/scratch/

rsync -rvltoD /Users/mb3324/Desktop/irp-mb3324/01-wildfire mb3324@dtn-b.hpc.ic.ac.uk:/rds/general/user/mb3324/home/

rsync -rvltoD mb3324@dtn-b.hpc.ic.ac.uk:/rds/general/user/mb3324/home/01-wildfire /Users/mb3324/Desktop/irp-mb3324/

This is done on dtn (data transfer node) to reduce clutter on login node.
Could also use scp -r instead of rsync -rvltoD, but rsync will sync the folders and transfer changes.

rsync -rvltoD /Users/mb3324/Desktop/elmfire/docker_shared_folder/01-dataset-realistic-over102 mb3324@dtn-b.hpc.ic.ac.uk:/rds/general/user/mb3324/home/elmfire

rsync -rvltoD mb3324@dtn-b.hpc.ic.ac.uk:/rds/general/user/mb3324/home/elmfire/docker_shared_folder /Users/mb3324/Desktop/elmfire/

---

## Anaconda/Conda
**Official guide:** https://icl-rcs-user-guide.readthedocs.io/en/latest/hpc/applications/guides/conda/
### Initial Setup (One-time):
```bash
module load miniforge/3
miniforge-setup
```
### In every session:
Activating conda and the environment of choice (defaults to base).
```bash
eval "$(~/miniforge3/bin/conda shell.bash hook)"
conda activate pytorch_env
```
This usually goes in the job script.
Can also disable auto activation of base env: conda config --set auto_activate_base false.
### Creating a Conda Environment
```bash
conda create -n hpcenv python=3.10
conda activate hpcenv
conda install numpy scipy matplotlib
```
**You can use pip inside a conda environment on the HPC when packages are not available via conda.**
---

FOR ELMFIRE ON HPC: (jq, rasterio, elmfire specific packages, etc.)
activate conda:
eval "$(~/miniforge3/bin/conda shell.bash hook)"
conda activate pytorch_env
conda install -c conda-forge jq
pip install --user google-api-python-client grpcio grpcio-tools python-dateutil

## **Checking Data Usage, Directory, and Job Status**
```bash
quota -s         # Check your disk quota
pwd              # Check your current directory (HOME or EPHEMERAL)
qstat -u mb3324  # Check your job status
```
---

## 📝 Editing Files on HPC

- **View only:** `view filename` or `vim -R filename` (exit with `:q`)
- **Edit:** `vim filename` (press `i` to insert, `Esc` to exit insert mode, `:wq` to save and quit)

---

## Submitting a Training Job

### Example PBS Script (`train_job.pbs`):

```bash
#!/bin/bash
#PBS -N butterfly_train
#PBS -l walltime=01:00:00
#PBS -l select=1:ncpus=4:mem=16gb
#PBS -j oe

eval "$(~/miniforge3/bin/conda shell.bash hook)"
conda activate pytorch_env

cd $PBS_O_WORKDIR
python train.py
```

### Submit:

```bash
qsub train_job.pbs
```

### Monitor Job:

```bash
qstat -u your_username
```

### Delete Job:

```bash
qdel <jobid>
```

---

## 💾 Checkpoints and Output

- Store outputs and models in your home or `/rds/general/user/<your_username>/home/project/`
- Avoid writing to shared folders without permission
- Save checkpoints every few epochs, and clean old files:

```bash
mkdir -p ~/project/checkpoints
python train.py --save_dir ~/project/checkpoints
```

---

## ⚠️ Warnings & Best Practices

- ❌ **Don’t run training on login nodes** — always use PBS jobs or interactive jobs (`qsub -I`).
- ❌ Don’t use `/scratch` or `$EPHEMERAL` unless you understand cleanup behavior.
- ❌ Don’t fill shared directories or exceed your quota.
- ✅ Always save data to `~/` or `/rds/general/user/<your_username>/home/`
- ⚠️ Avoid `conda init` inside the `.bashrc` — use the recommended `eval "$(...)"` line each session.
- ✅ Use `qstat`, not `top` or `htop`, to monitor HPC jobs.

---

## ✅ Example Workflow

1. SSH into HPC using terminal or VSCode Remote SSH
2. Activate your Conda env
3. Run training with PBS:
   ```bash
   qsub train_job.pbs
   ```
4. Monitor with `qstat`
5. Once training is done, download outputs:
   ```bash
   scp -r your_username@login.hpc.ic.ac.uk:~/project/outputs ./outputs
   ```
6. Done!

---

## 📚 More Help

- Official guide: [https://icl-rcs-user-guide.readthedocs.io/en/latest/hpc/](https://icl-rcs-user-guide.readthedocs.io/en/latest/hpc/)
- GitHub examples: [https://github.com/a-pouplin/use_HPC](https://github.com/a-pouplin/use_HPC)
- Drop-in support: Tuesdays 14:00–16:00, Room 402, Sherfield Building



# Jupyter HPC
# Jupyter
The Jupyter Notebook enables users to create documents that combine live code with narrative text, mathematical equations, visualizations, interactive controls, and other rich output. It also provides building blocks for interactive computing with data: a file browser, terminals, and a text editor. With the evolution of Jupyter notebook to Jupyter Lab, this expands most of the existing features which enable you to use text editors, terminals, data file viewers, and other custom components side by side with notebooks in a tabbed work area.

## JupyterHub
JupyterHub is a web service enabling multiple users to run their Jupyter Notebooks on shared resources. The Imperial College RCS team provide a JupyterHub service which runs interactive Jupyter notebook sessions on HPC hardware by running them as "batch" jobs. The URL for the JupyterHub service is:

https://jupyter.cx3.rcs.ic.ac.uk

You must be registered with the HPC service in order to use the JupyterHub service.

Warning

If you are prompted with an "Internal Server Error" when trying to sign in, you should first login to any Microsoft product such as Outlook or Office, with your Imperial account in the same browser window, then try to sign in again.

Moving to the new Jupyterhub service
If you're coming from our older system using our older jupyter service you may notice a few differences.

This service better manages resource usage. Both memory and CPU limits are controlled and restricted to the amount of resource that you request when you start your job. Should you go over these limits, you will see a message that your kernel has been killed and that it will restart. If you need more resource, you can request more in the server spawn menu, up to 8 cores and 64GB of RAM. If you need even more resource than this, you should submit a batch job.

Stopping a session
To stop a session, you need to go to File > Hub Control Panel > Stop My Server. The session might take a few seconds to properly end.

### Load custom Packages/Modules in JupyterHub
Whilst the default kernels available in Jupyter, whether Python or R, offer a certain number of base packages, users normally want to use their own custom packages within their programs. The good news is that Jupyter integrates perfectly with conda and its package and environment management systems.

The recommended way to work with python and R packages is via conda. This also enables users to create segregated environments for different projects. More information on how to use conda environments can be found here.

### Python
More information on how to use Python with conda can be found in the Python application guide.

To use custom python modules within a Jupyter lab session:

On login node run

Load module
eval "$(~/miniforge3/bin/conda shell.bash hook)"
Setup a new conda environment for this project. I named this environment "test1" as an example. * Note that I specify a couple of starting packages to be installed: ipykernel and python with a specific version.
conda create -n test1 python=3.9 ipykernel jupyter_client
Activate the environment:
source activate test1
Install desired packages:
conda search "package_name"
conda install package_name[=version]
Install python kernel for Jupyter:
python -m ipykernel install --user --name python39_test1 --display-name "Python3.9 (test1)"
On JupyterHub

Start a new Jupyter Hub session.
Select the new "Python3.9 (test1)" icon in the Jupyter Launcher.
This will enable you to access all python modules in the conda environment created. In this case "test1".

### Connecting VSCode to a Jupyterhub session
It's possible to use VSCode to connect to a running JupyterHub session. Please follow the steps below to do so:

JupyterHub Steps
Login to JupyterHub and navigate to the token page
Set a note and token expiration if you want, then press "Request new API token". Make a note of this and store it securely. If you lose it, or it expires, delete it and create a new one.
Start up a Jupyter session with the necessary resource requirements from here
VSCode Steps
Open VSCode and install the official Microsoft Jupyter and JupyterHub extensions
Create a new notebook by opening the command palette (Ctrl+Shit+P on Windows, Cmd+Shift+P on MacOS), then type in and select the option "Create: New Jupyter Notebook"
Open command palette again and search for the option "Notebook: Select Notebook Kernel" > "Select Another Kernel" > "Existing JupyterHub Server".
Then paste in the url https://jupyter.cx3.rcs.ic.ac.uk
As prompted enter your Imperial username, then paste in the token you previously copied. You may then be prompted to set the name of the server.
Select your kernel from the drop down. That's it, you're done!
If you have any issues with that, you can get in touch with us.

Using previously created environments on the new service
We have identified that at some point the behaviour of Jupyter kernels have changed. As of now, kernels are stored in .local inside your $HOME directories meaning some Conda environments that may have worked in the previous jupyter will not immediately carry over to the new one.

If you cannot see your previously created environments on the new Jupyterhub service, do this:

First, activate the environment you wish to use:

[user@login ~]$ eval "$(~/miniforge3/bin/conda shell.bash hook)"
[user@login ~]$ conda activate pytorch_env
Then run the following:

[user@login ~]$ python -m ipykernel install --user --name python39_torch_env --display-name "Python3.9 (torch)"
You can of course change --display-name to whatever you'd like, and the --name should be the same as your Conda environment name. Y