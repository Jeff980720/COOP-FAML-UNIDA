const pool = require('../database/connection');

const getUsuarios = async (req, res) => {
    try {
        const result = await pool.query('SELECT idusuario, username, nombre_completo, rol, estado FROM usuarios ORDER BY idusuario ASC');
        res.json(result.rows);
    } catch (error) {
        res.status(500).json({ msg: 'Error al obtener usuarios' });
    }
};

const crearUsuario = async (req, res) => {
    // Extraemos 'adminPassword' que viene del SweetAlert
    const { username, password, nombre_completo, rol, contrasenia } = req.body;

    try {
        // IMPORTANTE: Verifica que req.usuarioId esté llegando desde el middleware validarJWT
        if (!req.usuarioId) {
            return res.status(401).json({ msg: 'Token no válido o falta ID de administrador' });
        }
        // 1. Validar la contraseña del administrador (asumiendo que el ID del admin está en req.usuarioId por el JWT)
        const adminQuery = await pool.query('SELECT password FROM usuarios WHERE idusuario = $1', [req.usuarioId]);
        const adminDB = adminQuery.rows[0];

        // Comparación (Si usas bcrypt, usa bcrypt.compareSync)
        if (contrasenia !== adminDB.password) {
            return res.status(401).json({ msg: 'Contraseña de administrador incorrecta' });
        }

        // 2. Proceder con la inserción si la clave es correcta
        await pool.query(
            'INSERT INTO usuarios (username, password, nombre_completo, rol) VALUES ($1, $2, $3, $4)',
            [username, password, nombre_completo, rol]
        );
        res.json({ msg: 'Usuario creado con éxito' });
    } catch (error) {
        res.status(500).json({ msg: 'Error al crear usuario' });
    }
};

const actualizarUsuario = async (req, res) => {
    const { id } = req.params;
    const { username, nombre_completo, rol, estado, contrasenia } = req.body;

    try {
        // IMPORTANTE: Verifica que req.usuarioId esté llegando desde el middleware validarJWT
        if (!req.usuarioId) {
            return res.status(401).json({ msg: 'Token no válido o falta ID de administrador' });
        }
        // 1. Verificar identidad del administrador
        const adminQuery = await pool.query('SELECT password FROM usuarios WHERE idusuario = $1', [req.usuarioId]);
        if (contrasenia !== adminQuery.rows[0].password) {
            return res.status(401).json({ msg: 'No autorizado: Clave administrativa errónea' });
        }

        // 2. Actualizar
        const result = await pool.query(
            'UPDATE usuarios SET username = $1, nombre_completo = $2, rol = $3, estado = $4 WHERE idusuario = $5',
            [username, nombre_completo, rol, estado, id]
        );

        if (result.rowCount === 0) return res.status(404).json({ msg: 'Usuario no encontrado' });

        res.json({ msg: 'Usuario actualizado correctamente' });
    } catch (error) {
        res.status(500).json({ msg: 'Error al actualizar usuario' });
    }
};

const eliminarUsuario = async (req, res) => {
    const { id } = req.params;
    const { contrasenia } = req.body;

    try {
        // IMPORTANTE: Verifica que req.usuarioId esté llegando desde el middleware validarJWT
        if (!req.usuarioId) {
            return res.status(401).json({ msg: 'Token no válido o falta ID de administrador' });
        }
        // 1. Verificar clave del administrador
        const adminQuery = await pool.query('SELECT password FROM usuarios WHERE idusuario = $1', [req.usuarioId]);
        if (contrasenia !== adminQuery.rows[0].password) {
            return res.status(401).json({ msg: 'Contraseña incorrecta, eliminación cancelada' });
        }

        // 2. Borrado físico
        const result = await pool.query('DELETE FROM usuarios WHERE idusuario = $1', [id]);

        if (result.rowCount === 0) return res.status(404).json({ msg: 'Usuario no encontrado' });

        res.json({ msg: 'Usuario eliminado con éxito' });
    } catch (error) {
        res.status(500).json({ msg: 'Error al eliminar usuario' });
    }
};

// No olvides exportarlos todos
module.exports = {
    getUsuarios,
    crearUsuario,
    actualizarUsuario,
    eliminarUsuario
};