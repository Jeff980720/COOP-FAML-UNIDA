# Imagen base
FROM node:20-alpine

# Directorio de trabajo
WORKDIR /app

# Copiar dependencias e instalarlas
COPY package*.json ./
RUN npm install

# Copiar el resto del código
COPY . .

# Exponer el puerto del backend
EXPOSE 3000

# Comando de inicio
CMD ["node", "index.js"]
