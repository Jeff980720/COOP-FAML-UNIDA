const pool = require('../database/connection');
const jwt = require('jsonwebtoken');

const login = async (req, res) => {
    // Usamos 'username' para que coincida con tu tabla
    const { username, password } = req.body;

    try {
        // 1. Buscamos al usuario por su username y verificamos que esté activo (estado = true)
        const query = 'SELECT idusuario, username, password, nombre_completo, rol FROM usuarios WHERE username = $1 AND estado = TRUE';
        const result = await pool.query(query, [username]);

        if (result.rows.length === 0) {
            return res.status(400).json({ msg: 'Usuario no encontrado o cuenta desactivada' });
        }

        const user = result.rows[0];

        // 2. Validación de contraseña
        // Por ahora comparamos texto plano. Si luego usas bcrypt, aquí cambiaría la lógica.
        if (password !== user.password) {
            return res.status(400).json({ msg: 'Contraseña incorrecta' });
        }

        // 3. Generar el Token (JWT)
        const token = jwt.sign(
            { id: user.idusuario, rol: user.rol }, 
            process.env.JWT_SECRET || 'secret_jeff_1998', 
            { expiresIn: '8h' }
        );

        // 4. Respuesta al Frontend
        res.json({
            ok: true,
            id: user.idusuario,
            nombre: user.nombre_completo,
            username: user.username,
            rol: user.rol,
            token
        });

    } catch (error) {
        console.error("Error en login:", error.message);
        res.status(500).json({ msg: 'Hable con el administrador' });
    }
};

const renovarToken = async (req, res) => {
    const { idUsuario, rolUsuario } = req;

    // Generamos un nuevo token para extender la sesión del usuario
    const token = jwt.sign(
        { id: idUsuario, rol: rolUsuario }, 
        process.env.JWT_SECRET || 'jeff_clave_secreta_2026', 
        { expiresIn: '8h' }
    );

    res.json({
        ok: true,
        token
    });
};

module.exports = {
    login,
    renovarToken
};