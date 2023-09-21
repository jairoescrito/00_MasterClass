# Configuración y Conexión con Github

# Carga librería para uso de git
library(usethis)
# Activar Git
use_git()
# Activar GitHub
use_github()

### Si se requiere generar un nuevo Token ###
# Generar Token de conexión
create_github_token()
# Activar Token de Conexión
library(credentials)
set_github_pat()
