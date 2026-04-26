const express = require('express');
const { Pool } = require('pg');
const cors = require('cors');
require('dotenv').config();

const app = express();

// Middlewares
app.use(cors()); // Permite que Angular se conecte
app.use(express.json()); // Permite leer archivos JSON en las peticiones

// 3. Rutas (Endpoints)
app.use('/api/auth', require('./src/routes/auth.routes'));
app.use('/api/socios', require('./src/routes/socios.routes'));
app.use('/api/aportes', require('./src/routes/aportes.routes'));
const aportesRoutes = require('./src/routes/aportes.routes');
app.use('./api/aportes',aportesRoutes)
app.use('/api/prestamos', require('./src/routes/prestamos.routes'));
const prestamosRoutes = require('./src/routes/prestamos.routes');
app.use('/api/prestamos', prestamosRoutes);
app.use('/api/pagos', require('./src/routes/pagos.routes'));
const pagosRoutes = require('./src/routes/pagos.routes');
app.use('/api/pagos', pagosRoutes);
app.use('/api/caja', require('./src/routes/caja.routes'));
app.use('/api/reportes', require('./src/routes/contabilidad.routes'));
app.use('/api/usuarios', require('./src/routes/usuarios.routes'));

// 4. Configuración del Puerto
const PORT = process.env.PORT || 3000;

app.listen(PORT, () => {
    console.log('==============================================');
    console.log(`   SISTEMA CAJA DE AHORRO - BACKEND ACTIVO`);
    console.log(`   Puerto: ${PORT}`);
    console.log(`   Estado: Esperando peticiones...`);
    console.log('==============================================');
});




// // Iniciar servidor
// const PORT = process.env.PORT || 3000;
// app.listen(PORT, () => {
//     console.log(`Servidor de la Cooperativa corriendo en el puerto ${PORT}`);
// });