const { Router } = require('express');
const router = Router();
const { login, renovarToken } = require('../controllers/auth.controller');
const { validarJWT } = require('../middlewares/validar-jwt');

// POST /api/auth/login
router.post('/login', login);

// GET /api/auth/renew (Para mantener la sesión activa en Angular)
router.get('/renew', validarJWT, renovarToken);

module.exports = router;