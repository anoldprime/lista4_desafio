### Conectando à Conta do Google Cloud usando uma chave JSON ----

# Em caso de dúvidas, veja https://www.youtube.com/watch?v=1oM0NZbRhSI

# Carregando os pacotes
pacman::p_load("googleComputeEngineR", "googleAuthR", "ssh", "tictoc",
               "jsonlite", "furrr", "tidyverse", "credentials")

# Endereço do arquivo JSON baixado do Google Cloud
auth_file <- "topicos2-desafio-copa-70ef0475271d.json"

# Definindo uma variável de ambiente
Sys.setenv(GCE_AUTH_FILE = auth_file)

# Autenticando o usuário
gar_auth_service(auth_file, 
                 scope="https://www.googleapis.com/auth/cloud-platform")

# Definindo a zona onde a instância será iniciada
gce_global_zone("us-central1-a")

# Lembre-se de habilitar o uso do GCE no console do Google
# O ID do projeto pode ser consultado pelo console
gce_global_project("topicos2-desafio-copa")


### Criando uma instância remota (VM - Virtual Machine) e conectando via SSH ----
# Ao escolher o parametro preemptive=T, a máquina pode ser desligada pelo GCE
vm <- gce_vm(name = "computador-1",
             template = "rstudio",
             username = "login",
             password = "senha",
             predefined_type = "e2-standard-16",
             scheduling = list(preemptible = TRUE))

# Identificando se existem chaves publicas e privadas. Se não existir, crie-as
# Veja https://community.rstudio.com/t/ssh-to-linux-via-terminal/131455
# Clicar em Tools > Global options > Terminal > New terminals opens with Windows PowerShell
# Esse processo é necessário para permitir que a instância local se conecte à VM via SSH
ssh_key_info()

# Fazendo upload das chaves para o projeto do Google Cloud
vm <- gce_ssh_setup(vm,
                    key.pub = str_c(ssh_key_info()$key, ".pub"),
                    key.private = ssh_key_info()$key)

# Testando a conexão. Se der certo, o conando abaixo retornará o texto abaixo
# Usage: grep [OPTION]... PATTERN [FILE]... Try 'grep --help' for more information.  
gce_ssh(vm, "grep")
# Para instalar o R, por exemplo, poderia se usar o comando sudo apt -y install r-base 

# Se a linha abaixo não rodar, crie uma regra de excessão para o firewall no google cloud
# https://community.rstudio.com/t/unable-to-connect-to-r-studio-server-in-browser/24771/4
browseURL(paste0("http://", gce_get_instance(vm)$networkInterfaces$accessConfigs[[1]]$natIP))

# Para transferir arquivos entre o servidor e o PC, ver
# https://support.rstudio.com/hc/en-us/articles/200713893-Uploading-and-Downloading-Files

# Para fechar a instância, uso o console ou o comando 
gce_vm_delete("computador-1")

# Confira se a instância foi efetivamente deletada
gce_list_instances()


### Criando um cluster no GCE ----

# Criando o cluster
vms <- gce_vm_cluster(vm_prefix = "r-cluster-",
                      cluster_size = 3,
                      docker_image = "rocker/r-parallel",
                      predefined_type = "e2-standard-4",
                      scheduling = list(preemptible = TRUE), 
                      ssh_args = list(key.pub = str_c(ssh_key_info()$key, ".pub"),
                                      key.private = ssh_key_info()$key))

# Transformando em um objeto da classe Cluster
vms_as_cluster <- as.cluster(vms)

### Paralelizando a computação de uma função ----

# Definindo a função que queremos computar repetidas vezes
# Se a função envolver números aleatórios, use .options = furrr_options(seed=42)
func <- function(.) {
  Sys.sleep(.)
  Sys.time()
}

# Definindo os parâmetros sobre os quais queremos calcular a função
time_sleep <- rep(10, 6)

# Definindo a precisão da consulta do horário
op <- options(digits.secs = 2)

# Fazendo o calculo usando a função map (equivale a usar o plano "sequential")
tic()
map(time_sleep, ~ func(.))
toc()

# Paralelizando nos cores da máquina local
plan(multisession, workers = 6)
tic()
future_map(time_sleep, ~ func(.))
toc()

# Paralelizando nos cores de cada um dos nós do cluster
time_sleep_list <- list(node1 = time_sleep[1:2],
                        node2 = time_sleep[3:4],
                        node3 = time_sleep[5:6]) 
plan(cluster, workers = vms_as_cluster)
tic()
future_map(.x = time_sleep_list, 
           .f = ~ {
             plan(multisession, workers = 2)
             future_map(
               .x = .x,
               .f = ~ {
                 Sys.sleep(.x)
                 Sys.time()
               }
             )
           }, 
           .options = furrr_options(seed=42)
)
toc()

# Deletando as instâncias
gce_vm_delete(str_c("r-cluster-", 1:3))
gce_list_instances()
