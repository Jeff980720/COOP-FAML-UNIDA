# Etapa de build
FROM node:20-alpine AS build
WORKDIR /app
COPY package*.json ./
RUN npm install
COPY . .
RUN npm run build --prod
# AGREGA ESTA LÍNEA TEMPORALMENTE:
RUN ls -R dist/

# Etapa de producción
FROM nginx:alpine

# Asegúrate de que la ruta incluya /browser al final
# Esto copiará el contenido de browser si existe, o de Cooperativa si no.
COPY --from=build /app/dist/Cooperativa/browser/. /usr/share/nginx/html/

# 2. Copia tu configuración para que no salga el "Welcome to nginx"
COPY nginx.conf /etc/nginx/conf.d/default.conf

EXPOSE 80