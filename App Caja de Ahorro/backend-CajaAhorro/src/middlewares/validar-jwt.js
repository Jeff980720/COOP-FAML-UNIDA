const jwt = require('jsonwebtoken');

const validarJWT = (req, res, next) => {
    // Leer el token del header 'x-token' (o 'Authorization')
    const token = req.header('x-token');

    if (!token) {
        return res.status(401).json({ msg: 'No hay token en la petición' });
    }

    try {
        const { id, rol } = jwt.verify(token, process.env.JWT_SECRET || 'secret_jeff_1998');

        // Guardamos los datos del usuario en la petición para usarlos si es necesario
        // req.idUsuario = id;
        req.usuarioId = id;
        req.rolUsuario = rol;

        next(); // ¡Pasa el guardia!
    } catch (error) {
        return res.status(401).json({ msg: 'Token no válido' });
    }
};

module.exports = { validarJWT };